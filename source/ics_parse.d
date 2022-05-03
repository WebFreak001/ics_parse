/**
Parser for .ics files (ical format)

Only supports parsing VEVENT right now.
(no VTODO, VJOURNAL, VFREEBUSY, VTIMEZONE, iana components, custom
X-components)

Does not support non-UTC timezones.

For an entry look at the $(LREF parseCalendar) function.

Standards: https://datatracker.ietf.org/doc/html/rfc5545

History:
	May 2022 - added support for date/datetime value type
		(DTSTART;VALUE=DATE:20220503) parsing

	June 2019 - initial version
*/
module ics_parse;

import core.lifetime;
import core.time;

import std.algorithm;
import std.array;
import std.ascii;
import std.conv;
import std.datetime.date;
import std.datetime.systime;
import std.datetime.timezone;
import std.meta : staticIndexOf;
import std.range;
import std.string;
import std.sumtype;
import std.traits;

/// Specifies a calendar, which is basically a full ical file.
/// A calendar consists of some metadata and an arbitrary amount of events.
/// Examples: https://datatracker.ietf.org/doc/html/rfc5545#section-4
struct VCalendar
{
	/// Contains the spec version if set.
	/// Only supported version: 2.0
	/// Standards: https://datatracker.ietf.org/doc/html/rfc5545#section-3.7.4
	string specVersion;
	/// Contains the "PRODID" product identifier of the calendar metadata.
	/// Standards: https://datatracker.ietf.org/doc/html/rfc5545#section-3.7.3
	string productId;
	/// Contains the "CALSCALE" calendar scale of the calendar metadata.
	/// Standards: https://datatracker.ietf.org/doc/html/rfc5545#section-3.7.1
	string scale = "GREGORIAN";
	/// Contains the "METHOD" of the calendar metadata.
	/// Standards: https://datatracker.ietf.org/doc/html/rfc5545#section-3.7.2
	string method;
	/// Contains any "X-" prefixed custom properties or unimplemented properties.
	XProp[] xProps;

	/// Contains all the supported `BEGIN:*`, `END:*` blocks.
	CalendarEntry[] entries;
}

/// Combines supported calendar entry types as SumType.
///
/// The null type is emitted in the error handler and for unimplemented
/// component types. (VTODO, VJOURNAL, VFREEBUSY, VTIMEZONE, iana components,
/// "X-" custom components)
///
/// Currently supported:
/// - VEvent
alias CalendarEntry = SumType!(typeof(null), VEvent);

/// Specifies an iCal event component
/// Standards: https://datatracker.ietf.org/doc/html/rfc5545#section-3.6.1
struct VEvent
{
	/// Contains the required "UID" value
	/// Standards: https://datatracker.ietf.org/doc/html/rfc5545#section-3.8.4.7
	string uid;
	/// Contains the required "DTSTAMP" value
	/// Standards: https://datatracker.ietf.org/doc/html/rfc5545#section-3.8.7.2
	SysTime dtStamp;
	/// Contains the optional Date-time "CREATED" value
	/// Standards: https://datatracker.ietf.org/doc/html/rfc5545#section-3.8.7.1
	SysTime created;
	/// Contains the semi-required "DTSTART" value
	/// `null` means it's not set, T.init means there is possibly an error value.
	/// Standards: https://datatracker.ietf.org/doc/html/rfc5545#section-3.8.2.4
	SumType!(typeof(null), SysTime, Date) dtStart;
	/// Contains the optional "DTEND" value (not compatible with duration)
	/// `null` means it's not set, T.init means there is possibly an error value.
	/// Standards: https://datatracker.ietf.org/doc/html/rfc5545#section-3.8.2.2
	SumType!(typeof(null), SysTime, Date) dtEnd;
	/// Contains the optional "Duration" value (not compatible with dtEnd)
	/// Standards: https://datatracker.ietf.org/doc/html/rfc5545#section-3.8.2.5
	Duration duration;
	/// Contains the optional "CLASS" value
	/// Standards: https://datatracker.ietf.org/doc/html/rfc5545#section-3.8.1.3
	string classification = "PUBLIC";
	/// Contains the optional "LAST-MODIFIED" value
	/// Standards: https://datatracker.ietf.org/doc/html/rfc5545#section-3.8.7.3
	SysTime lastModified;
	/// Contains the optional "DESCRIPTION" value
	/// Standards: https://datatracker.ietf.org/doc/html/rfc5545#section-3.8.1.5
	string description;
	/// Contains the optional "SUMMARY" value
	/// Standards: https://datatracker.ietf.org/doc/html/rfc5545#section-3.8.1.12
	string summary;
	/// Contains the optional "GEO" position
	/// Standards: https://datatracker.ietf.org/doc/html/rfc5545#section-3.8.1.6
	double[2] geo = 0;
	/// Contains the optional "LOCATION" value
	/// Standards: https://datatracker.ietf.org/doc/html/rfc5545#section-3.8.1.7
	string location;
	/// Contains the optional "ORGANIZER" value
	/// Standards: https://datatracker.ietf.org/doc/html/rfc5545#section-3.8.4.3
	string organizer;
	/// Contains the optional "PRIORITY" value, 0 if unspecified or set to 0
	/// Standards: https://datatracker.ietf.org/doc/html/rfc5545#section-3.8.1.9
	int priority;
	/// Contains the optional "SEQUENCE" sequence number
	/// Standards: https://datatracker.ietf.org/doc/html/rfc5545#section-3.8.7.4
	int sequenceNumber;
	/// Contains the optional "STATUS" value
	/// Standards: https://datatracker.ietf.org/doc/html/rfc5545#section-3.8.1.11
	EventStatus status;
	/// Contains the optional "RRULE" recurrence rules
	/// There SHOULD NOT be more than one RecurrenceRule item in this array.
	/// If both $(LREF recurrenceRules) and $(LREF recurrenceDates) are set,
	/// take the union of dates, e.g. combine them together.
	/// Standards: https://datatracker.ietf.org/doc/html/rfc5545#section-3.8.5.3
	RecurrenceRule[] recurrenceRules;
	/// Contains the optional "TRANSP" time transparency value
	/// Standards: https://datatracker.ietf.org/doc/html/rfc5545#section-3.8.2.7
	TimeTransparency timeTransparency;
	/// Contains the optional "URL" value
	/// Standards: https://datatracker.ietf.org/doc/html/rfc5545#section-3.8.4.6
	string url;
	/// Contains the optional "RECURRENCE-ID" value
	/// Standards: https://datatracker.ietf.org/doc/html/rfc5545#section-3.8.4.4
	string recurrenceId;

	/// Contains the optional "ATTACH" attachment values, which are either URIs
	/// or embedded binary data.
	/// Standards: https://datatracker.ietf.org/doc/html/rfc5545#section-3.8.1.1
	Attachment[] attachments;
	/// Contains the optional "ATTENDEE" values
	/// Standards: https://datatracker.ietf.org/doc/html/rfc5545#section-3.8.4.1
	Attendee[] attendees;
	/// Contains the optional "CATEGORIES" values
	/// Standards: https://datatracker.ietf.org/doc/html/rfc5545#section-3.8.1.2
	string[] categories;
	/// Contains the optional "COMMENT" values
	/// Standards: https://datatracker.ietf.org/doc/html/rfc5545#section-3.8.1.4
	string[] comments;
	/// Contains the optional "CONTACT" values
	/// Standards: https://datatracker.ietf.org/doc/html/rfc5545#section-3.8.4.2
	string[] contacts;
	/// Contains the optional "EXDATE" recurrence date exception values
	/// Standards: https://datatracker.ietf.org/doc/html/rfc5545#section-3.8.5.1
	SumType!(SysTime, Date)[] exceptionDates;
	/// Contains the optional "RDATE" recurrence date values
	/// If both $(LREF recurrenceRules) and $(LREF recurrenceDates) are set,
	/// take the union of dates, e.g. combine them together.
	/// Standards: https://datatracker.ietf.org/doc/html/rfc5545#section-3.8.5.2
	SumType!(SysTime, Date, Period)[] recurrenceDates;
	/// Contains the optional "REQUEST-STATUS" values
	/// Standards: https://datatracker.ietf.org/doc/html/rfc5545#section-3.8.8.3
	RequestStatus[] requestStatus;
	/// Contains the optional "RELATED-TO" values
	/// Standards: https://datatracker.ietf.org/doc/html/rfc5545#section-3.8.4.5
	string[] relatedTo;
	/// Contains the optional "RESOURCES" values
	/// Standards: https://datatracker.ietf.org/doc/html/rfc5545#section-3.8.1.10
	string[] resources;
	/// Contains any "X-" prefixed custom properties or unimplemented properties.
	XProp[] xProps;

	/// Returns true if uid, dtStamp and dtStart are set.
	/// If the calendar for this event has a method set, dtStart is not
	/// required and the test should be done manually.
	bool valid() const @property
	{
		return uid.length && dtStamp != SysTime.init && dtStart != typeof(dtStart).init;
	}
}

///
alias Attachment = SumType!(string, const(ubyte)[]);

///
struct Attendee
{
	///
	string address;
}

///
enum EventStatus
{
	/// No value set
	none,
	/// corresponds to iCal value "TENTATIVE"
	tentative,
	/// corresponds to iCal value "CONFIRMED"
	confirmed,
	/// corresponds to iCal value "CANCELLED"
	cancelled
}

///
enum TimeTransparency
{
	/// No value set
	none,
	/// corresponds to iCal value "TRANSPARENT"
	/// Events, which do not take up the individual's (or resource's) time
	/// SHOULD be recorded as TRANSPARENT, making them invisible to free/
	/// busy time searches.
	transparent,
	/// corresponds to iCal value "OPAQUE"
	/// Events that consume actual time for the individual or resource
	/// associated with the calendar SHOULD be recorded as OPAQUE,
	/// allowing them to be detected by free/busy time searches
	opaque
}

///
struct RequestStatus
{
	///
	int statusCode;
	///
	string statusMessage;
	///
	string exceptionData;
}

/// describes a custom property parameter
struct XParam
{
	string name;
	string[] values;
}

/// describes a custom property
struct XProp
{
	string name;
	string value;
	XParam[] params;
	LanguageTag language;
}

///
struct LanguageTag
{
	string primaryTag;
	string[] subTags;
}

///
struct RecurrenceRule
{
	string[] ruleParts;
}

/// Defines a time period
struct Period
{
	///
	SysTime start, end;
}

private struct Tokenizer
{
	const(char)[] source;
	const(char)[] data;
	size_t[] unfoldedIndices;

	this(in char[] str)
	{
		auto copy = str.strip;
		source.reserve(copy.length);
		int step = 0;
		foreach (i, ubyte b; copy)
		{
			if (b == '\r')
				step = 1;
			else if (b == '\n' && step == 1)
				step = 2;
			else if ((b == ' ' || b == '\t') && step == 2)
			{
				step = 0;
				source = source[0 .. $ - 2].assumeSafeAppend;
				unfoldedIndices ~= i;
				continue;
			}
			else
				step = 0;

			source ~= cast(char)b;
		}
		data = source;
	}

	bool matchTokens(string[] tokens...)
	{
		auto orig = data;
		foreach (token; tokens)
		{
			if (!data.startsWithToken(token))
			{
				data = orig;
				return false;
			}
			data = data[token.length .. $];
		}
		return true;
	}

	bool matchCRLF()
	{
		if (data.startsWith("\r\n"))
		{
			data = data[2 .. $];
			return true;
		}
		else
		{
			return false;
		}
	}

	void skipOverOrEof(in char[] match)
	{
		auto end = data.countUntil(match);
		if (end == -1)
			data = data[$ .. $];
		else
			data = data[end + match.length .. $];
	}

	const(char)[] peekUntil(in char[] match) const
	{
		auto end = data.countUntil(match);
		if (end == -1)
			return data[0 .. $];
		else
			return data[0 .. end];
	}

	uint[2] lineAndColumn() const @property
	{
		auto i = data.ptr - source.ptr;
		int lineOffset;
		foreach (unfold; unfoldedIndices)
		{
			if (i + lineOffset * 3 >= unfold)
				lineOffset++;
			else
				break;
		}

		auto line = source[0 .. i].representation.count('\n') + 1 + lineOffset;
		auto lastNl = source[0 .. i].lastIndexOf('\n');
		auto column = lastNl == -1 ? 0 : (source[lastNl + 1 .. i].length + 1);

		return [cast(uint)line, cast(uint)column];
	}

	const(char)[] debugMsgCurrentLine() const @property
	{
		auto lf = data.representation.countUntil('\n');
		return lf == -1 ? data : data[0 .. lf].chomp("\r");
	}
}


unittest
{
	auto tok = Tokenizer(`
BEGIN:VCALENDAR
 
PRODID:Te
 stKalender
VERSION:2.0
`.splitLines.join("\r\n"));
	// test unfold
	assert(tok.source == `BEGIN:VCALENDAR
PRODID:TestKalender
VERSION:2.0`.splitLines.join("\r\n"), [tok.source].to!string);
	assert(tok.matchTokens("BEGIN", ":", "VCALENDAR"));
	assert(tok.matchCRLF);
	assert(tok.matchTokens("PRODID"));
	assert(tok.matchTokens(":"));
	assert(tok.matchTokens("TestKalender"));
	assert(tok.matchCRLF);
	assert(tok.matchTokens("VERSION", ":"));
	assert(tok.matchTokens("2.0"));
}


private bool startsWithToken(in char[] data, string token)
{
	if (!data.startsWith(token))
		return false;
	if (isIdentifier(token[$ - 1]) && data.length > token.length && isIdentifier(data[token.length]))
		return false;
	return true;
}

private bool isIdentifier(dchar c)
{
	return isAlpha(c) || c == '_';
}

private bool isTSafeChar(dchar c)
{
	// Any character except CTLs not needed by the current character set, DQUOTE, ";", ":", "\", ","
	return c == ' ' || c == '\t' || c == '\x21' || (c >= '\x23' && c <= '\x2B') || (c >= '\x2D'
			&& c <= '\x39') || (c >= '\x3C' && c <= '\x5B') || c >= '\x5D';
}

private bool isQSafeChar(dchar c)
{
	// Any character except CTLs and DQUOTE
	return isWSP(c) || c == '\x21' || c >= '\x23';
}

private bool isSafeChar(dchar c)
{
	// Any character except CTLs, DQUOTE, ";", ":", ","
	return isWSP(c) || c == '\x21' || (c >= '\x23' && c <= '\x2b') || (c >= '\x2d'
			&& c <= '\x39') || c >= '\x3c';
}

private bool isWSP(dchar c)
{
	return c == '\x20' || c == '\x09';
}

private string colonValueBoilerplate(string property, string currentEntry)
{
	if (__ctfe)
	{
		return `if (!tokens.matchTokens(":"))
		{
			error(` ~ (currentEntry.length ? currentEntry ~ ", " : "")
				~ `"` ~ property ~ `", "Expected value following `
				~ property ~ `");
			return true;
		}
		scope (exit)
		{
			if (!tokens.matchCRLF())
				error(` ~ (currentEntry.length ? currentEntry ~ ", " : "")
					~ `"` ~ property ~ `", "Missing CRLF after ` ~ property ~ ` value");
		}`;
	}
	else
		return null;
}

private string earlyRejectWithXParamsAndCRLF(string property, string currentEntry)
{
	if (__ctfe)
	{
		return `if (!tokens.matchTokens("` ~ property ~ `"))
			return false;
		while (tokens.matchTokens(";"))
			parseXParam(` ~ (currentEntry.length ? currentEntry ~ ", " : "")
				~ `"` ~ property ~ `");
		` ~ colonValueBoilerplate(property, currentEntry);
	}
	else
		return null;
}

/// Defines a parser error callback.
/// Params:
/// 	partial = the VCalendar how it looks right now, possibly containing the currently parsing entry or not.
/// 	info = error information about the current scope and location including a huamn readable message.
alias ParseErrorCallback = void delegate(ref VCalendar partial, ErrorInfo info);

///
struct ErrorInfo
{
	/// The source data passed into the parser that is being worked on
	const(char)[] data;
	/// The source line where the parsing error happend (1-based)
	uint line;
	/// The column where the parsing error happend (1-based or 0 if not determinable)
	uint column;
	/// the property key or component name (VEVENT, VCALENDAR, ...) the error is about
	string key;
	/// a human readable error description
	string message;
	/// if set to non-null, the error is about the specific entry, otherwise probably about the full calendar or some meta error.
	CalendarEntry entry;

	string toString() const @safe
	{
		auto ret = text("Error in line ", line, " in column ", column, " for key ", key, ": ", message);
		return entry.match!(
			(typeof(null) _) => ret,
			v => ret ~ "\n\npartial " ~ Unqual!(typeof(v)).stringof ~ ": " ~ v.to!string
		);
	}
}

/// Implementation of iCal parsing
struct ICalendarParser
{
	Tokenizer tokens;
	ParseErrorCallback onError = null;
	VCalendar ret;

	void error(string key, string message)
	{
		if (onError)
		{
			auto lac = tokens.lineAndColumn;
			onError(ret, ErrorInfo(tokens.source, lac[0], lac[1], key, message, CalendarEntry.init));
		}
	}

	void error(T)(auto ref T entry, string key, string message)
	{
		if (onError)
		{
			auto lac = tokens.lineAndColumn;
			static if (is(T : CalendarEntry))
				onError(ret, ErrorInfo(tokens.source, lac[0], lac[1], key, message, entry));
			else
				onError(ret, ErrorInfo(tokens.source, lac[0], lac[1], key, message, CalendarEntry(entry)));
		}
	}

	private bool parseOnce(alias method)(ref bool got, string key, Parameters!method args)
	{
		if (!method(args))
			return false;
		if (got)
		{
			static if (typeof(args).length
				&& (is(typeof(args[0]) : CalendarEntry)
					|| staticIndexOf!(typeof(args[0]), CalendarEntry.Types) != -1))
				error(CalendarEntry(args[0]), key, "Specified multiple " ~ key ~ " properties in "
					~ typeof(args[0]).stringof);
			else
				error(key, "Specified multiple " ~ key ~ " properties in VCALENDAR");
		}
		got = true;
		return true;
	}

	private string parseShortVCalendarEnumValueBeforeCRLF(string key, string default_, string[] values...)
	{
		foreach (val; values)
			if (tokens.matchTokens(val))
				return val;

		auto crlf = tokens.data.countUntil("\r\n");
		if (crlf == -1)
			crlf = tokens.data.length;
		auto version_ = tokens.data[0 .. crlf];
		tokens.data = tokens.data[crlf .. $];
		error(key, "Unknown value for property: " ~ version_.idup);
		return default_;
	}

	private string parseAnyValueBeforeCRLF()
	{
		auto crlf = tokens.data.countUntil("\r\n");
		if (crlf == -1)
			crlf = tokens.data.length;
		auto ret = tokens.data[0 .. crlf].idup;
		tokens.data = tokens.data[crlf .. $];
		return ret;
	}

	VCalendar parseCalendar()
	{
		ret = VCalendar.init;
		if (!tokens.matchTokens("BEGIN", ":", "VCALENDAR"))
			return VCalendar.init;

		bool gotProdId, gotVersion, gotCalScale, gotMethod;
		while (true)
		{
			tokens.matchCRLF();
			//dfmt off
			if (parseOnce!parseProdId(gotProdId, "PRODID", ret)
			 || parseOnce!parseVersion(gotVersion, "VERSION", ret)
			 || parseOnce!parseCalScale(gotCalScale, "CALSCALE", ret)
			 || parseOnce!parseMethod(gotMethod, "METHOD", ret))
			{
			}
			//dfmt on
			else
			{
				auto prop = parseXProp();
				if (prop == XProp.init)
					break;
				else
					ret.xProps ~= prop;
			}
		}
		tokens.matchCRLF();

		if (!gotProdId)
			error("PRODID", "Did not get required PRODID for VCALENDAR");
		if (!gotVersion)
			error("VERSION", "Did not get required VERSION for VCALENDAR");

		while (tokens.matchTokens("BEGIN", ":"))
		{
			auto crlf = tokens.data.countUntil("\r\n");
			if (crlf == -1)
				crlf = cast(ptrdiff_t) tokens.data.length;
			auto type = tokens.data[0 .. crlf];
			tokens.data = tokens.data[crlf .. $];

			if (!tokens.matchCRLF)
			{
				auto typeDup = type.idup;
				error(typeDup, "Missing CRLF following START:" ~ typeDup);
			}

			if (type == "VEVENT")
			{
				ret.entries ~= CalendarEntry(parseVEventProps);
				if (!tokens.matchTokens("END", ":", "VEVENT"))
				{
					error(ret.entries[$ - 1], "VEVENT", text("Expected END:VEVENT but got instead: ", tokens.debugMsgCurrentLine));
					tokens.skipOverOrEof("END:VEVENT");
				}
				if (!tokens.matchCRLF)
					error(ret.entries[$ - 1], "VEVENT", "Missing CRLF following END:VEVENT");
			}
			else
			{
				auto typeDup = type.idup;
				// TODO: add VTODO, VJOURNAL, VFREEBUSY, VTIMEZONE, iana components, X components
				error(typeDup, "component type " ~ typeDup ~ " not implemented");
				ret.entries ~= CalendarEntry(null);

				tokens.skipOverOrEof("END:" ~ type);
				if (!tokens.matchCRLF)
					error(typeDup, "Missing CRLF following END:" ~ typeDup);
			}
		}

		tokens.matchCRLF();
		if (!tokens.matchTokens("END", ":", "VCALENDAR"))
			error("VCALENDAR", "Expected END:VCALENDAR, but didn't get it.");
		return ret;
	}

	VEvent parseVEventProps()
	{
		VEvent ret;

		bool gotClass, gotCreated, gotDescription, gotDtStamp, gotLastModified,
			gotDtStart, gotDtEnd, gotDuration, gotGeo, gotLocation,
			gotOrganizer, gotPriority, gotSeq, gotStatus, gotTransp, gotUrl,
			gotRecurrenceId, gotSummary, gotUid;
		while (true)
		{
			//dfmt off
			if (parseOnce!parseEventClass(gotClass, "CLASS", ret)
			 || parseOnce!parseEventCreated(gotCreated, "CREATED", ret)
			 || parseOnce!parseEventDescription(gotDescription, "DESCRIPTION", ret)
			 || parseOnce!parseEventDtStamp(gotDtStamp, "DTSTAMP", ret)
			 || parseOnce!parseEventLastModified(gotLastModified, "LAST-MODIFIED", ret)
			 || parseOnce!parseEventDtStart(gotDtStart, "DTSTART", ret)
			 || parseOnce!parseEventDtEnd(gotDtEnd, "DTEND", ret)
			 || parseOnce!parseEventDuration(gotDuration, "DURATION", ret)
			 || parseOnce!parseEventGeo(gotGeo, "GEO", ret)
			 || parseOnce!parseEventLocation(gotLocation, "LOCATION", ret)
			 || parseOnce!parseEventOrganizer(gotOrganizer, "ORGANIZER", ret)
			 || parseOnce!parseEventPriority(gotPriority, "PRIORITY", ret)
			 || parseOnce!parseEventSequence(gotSeq, "SEQUENCE", ret)
			 || parseOnce!parseEventStatus(gotStatus, "STATUS", ret)
			 || parseOnce!parseEventTimeTransparency(gotTransp, "TRANSP", ret)
			 || parseOnce!parseEventUrl(gotUrl, "URL", ret)
			 || parseOnce!parseEventRecurrenceId(gotRecurrenceId, "RECURRENCE-ID", ret)
			 || parseOnce!parseEventSummary(gotSummary, "SUMMARY", ret)
			 || parseOnce!parseEventUid(gotUid, "UID", ret)
			 // may be more than once:
			 || parseEventRecurrenceRule(ret)
			 || parseEventAttachment(ret)
			 || parseEventAttendee(ret)
			 || parseEventCategories(ret)
			 || parseEventComment(ret)
			 || parseEventContact(ret)
			 || parseEventExceptionDates(ret)
			 || parseEventRecurrenceDates(ret)
			 || parseEventRequestStatus(ret)
			 || parseEventRelatedTo(ret)
			 || parseEventResources(ret))
			{
			}
			//dfmt on
			else if (tokens.matchTokens("BEGIN", ":", "VALARM"))
			{
				error(ret, "VALARM", "VEVENT.VALARM not implemented");
				tokens.skipOverOrEof("END:VALARM");
				if (!tokens.matchCRLF)
					error(ret, "VALARM", "Missing CRLF following END:VALARM");
			}
			else
			{
				auto prop = parseXProp(ret);
				if (prop == XProp.init)
					break;
				else
					ret.xProps ~= prop;
			}
		}
		return ret;
	}

	bool parseProdId(ref VCalendar calendar)
	{
		mixin(earlyRejectWithXParamsAndCRLF("PRODID", null));
		calendar.productId = parseText("PRODID");
		return true;
	}

	bool parseVersion(ref VCalendar calendar)
	{
		mixin(earlyRejectWithXParamsAndCRLF("VERSION", null));
		calendar.specVersion = parseShortVCalendarEnumValueBeforeCRLF("VERSION", "2.0", "2.0");
		return true;
	}

	bool parseCalScale(ref VCalendar calendar)
	{
		mixin(earlyRejectWithXParamsAndCRLF("CALSCALE", null));
		calendar.scale = parseShortVCalendarEnumValueBeforeCRLF("CALSCALE", "GREGORIAN", "GREGORIAN");
		return true;
	}

	bool parseMethod(ref VCalendar calendar)
	{
		mixin(earlyRejectWithXParamsAndCRLF("METHOD", null));
		calendar.method = parseAnyValueBeforeCRLF();
		return true;
	}

	bool parseEventClass(ref VEvent event)
	{
		mixin(earlyRejectWithXParamsAndCRLF("CLASS", "event"));
		event.classification = parseAnyValueBeforeCRLF();
		return true;
	}

	bool parseEventCreated(ref VEvent event)
	{
		mixin(earlyRejectWithXParamsAndCRLF("CREATED", "event"));
		event.created = parseDateTime(event, "CREATED");
		return true;
	}

	bool parseEventLastModified(ref VEvent event)
	{
		mixin(earlyRejectWithXParamsAndCRLF("LAST-MODIFIED", "event"));
		event.lastModified = parseDateTime(event, "LAST-MODIFIED");
		return true;
	}

	bool parseEventDescription(ref VEvent event)
	{
		// TODO: support altrepparam & languageparam
		mixin(earlyRejectWithXParamsAndCRLF("DESCRIPTION", "event"));
		event.description = parseText(event, "DESCRIPTION");
		return true;
	}

	bool parseEventDtStamp(ref VEvent event)
	{
		mixin(earlyRejectWithXParamsAndCRLF("DTSTAMP", "event"));
		event.dtStamp = parseDateTime(event, "DTSTAMP");
		return true;
	}

	bool parseEventDtStart(ref VEvent event)
	{
		// TODO: support tzidparam
		if (!tokens.matchTokens("DTSTART"))
			return false;
		event.dtStart = parseTypedDateTime!(typeof(null), SysTime, Date)("DTSTART", event);

		return true;
	}

	bool parseEventDtEnd(ref VEvent event)
	{
		// TODO: support tzidparam
		if (!tokens.matchTokens("DTEND"))
			return false;
		event.dtEnd = parseTypedDateTime!(typeof(null), SysTime, Date)("DTEND", event);

		return true;
	}

	bool parseEventDuration(ref VEvent event)
	{
		mixin(earlyRejectWithXParamsAndCRLF("DURATION", "event"));
		event.duration = parseDuration(event, "DURATION");
		return true;
	}

	bool parseEventGeo(ref VEvent event)
	{
		mixin(earlyRejectWithXParamsAndCRLF("GEO", "event"));
		string geo = parseAnyValueBeforeCRLF();
		auto semicolon = geo.indexOf(';');
		if (semicolon == -1)
			return true;
		else
		{
			try
			{
				double[2] geoV;
				geoV[0] = geo[0 .. semicolon].to!double;
				geoV[1] = geo[semicolon + 1 .. $].to!double;
				event.geo = geoV;
			}
			catch (ConvException e)
			{
				error(event, "GEO", "failed parsing GEO value: " ~ e.msg);
			}
			return true;
		}
	}

	bool parseEventLocation(ref VEvent event)
	{
		// TODO: support altrepparam & languageparam
		mixin(earlyRejectWithXParamsAndCRLF("LOCATION", "event"));
		event.location = parseText(event, "LOCATION");
		return true;
	}

	bool parseEventOrganizer(ref VEvent event)
	{
		// TODO: support cnparam, dirparam, sentbyparam & languageparam 
		mixin(earlyRejectWithXParamsAndCRLF("ORGANIZER", "event"));
		event.organizer = parseText(event, "ORGANIZER");
		return true;
	}

	bool parseEventPriority(ref VEvent event)
	{
		mixin(earlyRejectWithXParamsAndCRLF("PRIORITY", "event"));
		try
		{
			event.priority = parseAnyValueBeforeCRLF().to!int;
		}
		catch (ConvException e)
		{
			error(event, "PRIORITY", "failed parsing PRIORITY value: " ~ e.msg);
		}
		return true;
	}

	bool parseEventSequence(ref VEvent event)
	{
		mixin(earlyRejectWithXParamsAndCRLF("SEQUENCE", "event"));
		try
		{
			event.sequenceNumber = parseAnyValueBeforeCRLF().to!int;
		}
		catch (ConvException e)
		{
			error(event, "SEQUENCE", "failed parsing SEQUENCE value: " ~ e.msg);
		}
		return true;
	}

	bool parseEventStatus(ref VEvent event)
	{
		mixin(earlyRejectWithXParamsAndCRLF("STATUS", "event"));
		auto s = parseAnyValueBeforeCRLF();
		switch (s)
		{
		case "TENTATIVE":
			event.status = EventStatus.tentative;
			break;
		case "CONFIRMED":
			event.status = EventStatus.confirmed;
			break;
		case "CANCELLED":
			event.status = EventStatus.cancelled;
			break;
		default:
			error(event, "STATUS", "invalid event status \"" ~ s ~ "\"");
			break;
		}
		return true;
	}

	bool parseEventTimeTransparency(ref VEvent event)
	{
		mixin(earlyRejectWithXParamsAndCRLF("TRANSP", "event"));
		auto s = parseAnyValueBeforeCRLF();
		switch (s)
		{
		case "OPAQUE":
			event.timeTransparency = TimeTransparency.opaque;
			break;
		case "TRANSPARENT":
			event.timeTransparency = TimeTransparency.transparent;
			break;
		default:
			error(event, "TRANSP", "invalid event time transparency \"" ~ s ~ "\"");
			break;
		}
		return true;
	}

	bool parseEventUrl(ref VEvent event)
	{
		mixin(earlyRejectWithXParamsAndCRLF("URL", "event"));
		event.url = parseAnyValueBeforeCRLF;
		return true;
	}

	bool parseEventRecurrenceId(ref VEvent event)
	{
		mixin(earlyRejectWithXParamsAndCRLF("RECURRENCE-ID", "event"));
		event.recurrenceId = parseText(event, "RECURRENCE-ID");
		return true;
	}

	bool parseEventSummary(ref VEvent event)
	{
		// TODO: support altrepparam & languageparam
		mixin(earlyRejectWithXParamsAndCRLF("SUMMARY", "event"));
		event.summary = parseText(event, "SUMMARY");
		return true;
	}

	bool parseEventUid(ref VEvent event)
	{
		mixin(earlyRejectWithXParamsAndCRLF("UID", "event"));
		event.uid = parseText(event, "UID");
		return true;
	}

	bool parseEventRecurrenceRule(ref VEvent event)
	{
		mixin(earlyRejectWithXParamsAndCRLF("RRULE", "event"));
		event.recurrenceRules ~= RecurrenceRule(parseAnyValueBeforeCRLF.split(";"));
		return true;
	}

	bool parseEventAttachment(ref VEvent event)
	{
		import std.base64;

		if (!tokens.matchTokens("ATTACH"))
			return false;
		bool b64;
		bool binary;
		while (tokens.matchTokens(";"))
		{
			if (tokens.matchTokens("ENCODING", "=", "BASE64"))
			{
				b64 = true;
			}
			else if (tokens.matchTokens("VALUE", "=", "BINARY"))
			{
				binary = true;
			}
			else
			{
				parseXParam(event, "ATTACH");
			}
		}
		mixin(colonValueBoilerplate("ATTACH", "event"));
		auto s = parseText(event, "ATTACH");
		if (b64 && binary)
			event.attachments ~= Attachment(Base64.decode(s));
		else if (b64)
			event.attachments ~= Attachment(cast(string) Base64.decode(s)); // newly allocated memory, safe to cast
		else if (binary)
			error(event, "ATTACH", "invalid attachment, has binary flag but not base64");
		else
			event.attachments ~= Attachment(s);
		return true;
	}

	bool parseEventAttendee(ref VEvent event)
	{
		if (!tokens.matchTokens("ATTENDEE"))
			return false;
		Attendee attendee;
		while (tokens.matchTokens(";"))
		{
			// TODO: cutypeparam, memberparam, roleparam, partstatparam, rsvpparam, deltoparam, delfromparam, sentbyparam, cnparam, dirparam, languageparam
			parseXParam(event, "ATTENDEE");
		}
		mixin(colonValueBoilerplate("ATTENDEE", "event"));
		attendee.address = parseText(event, "ATTENDEE");
		event.attendees ~= attendee;
		return true;
	}

	bool parseEventCategories(ref VEvent event)
	{
		// TODO: support languageparam
		mixin(earlyRejectWithXParamsAndCRLF("CATEGORIES", "event"));
		do
		{
			event.categories ~= parseText(event, "CATEGORIES");
		} while (tokens.matchTokens(","));
		return true;
	}

	bool parseEventComment(ref VEvent event)
	{
		// TODO: support altrepparam, languageparam
		mixin(earlyRejectWithXParamsAndCRLF("COMMENT", "event"));
		event.comments ~= parseText(event, "COMMENT");
		return true;
	}

	bool parseEventContact(ref VEvent event)
	{
		// TODO: support altrepparam, languageparam
		mixin(earlyRejectWithXParamsAndCRLF("CONTACT", "event"));
		event.contacts ~= parseText(event, "CONTACT");
		return true;
	}

	bool parseEventExceptionDates(ref VEvent event)
	{
		// TODO: support tzidparam
		if (!tokens.matchTokens("EXDATE"))
			return false;
		event.exceptionDates ~= parseTypedDateTimeArray!(SysTime, Date)("EXDATE", event);

		return true;
	}

	bool parseEventRecurrenceDates(ref VEvent event)
	{
		// TODO: support tzidparam
		if (!tokens.matchTokens("RDATE"))
			return false;
		event.recurrenceDates ~= parseTypedDateTimeArray!(SysTime, Date, Period)("RDATE", event);

		return true;
	}

	bool parseEventRequestStatus(ref VEvent event)
	{
		// TODO: support languageparam
		mixin(earlyRejectWithXParamsAndCRLF("REQUEST-STATUS", "event"));
		int num;
		try
		{
			num = parseText(event, "REQUEST-STATUS").to!int;
		}
		catch (ConvException e)
		{
			error(event, "REQUEST-STATUS", "failed parsing REQUEST-STATUS status code: " ~ e.msg);
		}
		auto desc = parseText(event, "REQUEST-STATUS");
		string err;
		if (tokens.matchTokens(";"))
			err = parseText(event, "REQUEST-STATUS");
		event.requestStatus ~= RequestStatus(num, desc, err);
		return true;
	}

	bool parseEventRelatedTo(ref VEvent event)
	{
		// TODO: support reltypeparam
		mixin(earlyRejectWithXParamsAndCRLF("RELATED-TO", "event"));
		event.relatedTo ~= parseText(event, "RELATED-TO");
		return true;
	}

	bool parseEventResources(ref VEvent event)
	{
		// TODO: support altrepparam, languageparam
		mixin(earlyRejectWithXParamsAndCRLF("RESOURCES", "event"));
		do
		{
			event.resources ~= parseText(event, "RESOURCES");
		} while (tokens.matchTokens(","));
		return true;
	}

	XProp parseXProp(T...)(auto ref T errorInfo)
	{
		auto recover = tokens.data;
		XProp ret;
		ret.name = parseXName;
		if (ret.name == "BEGIN" || ret.name == "END")
		{
			tokens.data = recover;
			return XProp.init;
		}
		while (tokens.matchTokens(";"))
		{
			if (tokens.matchTokens("LANGUAGE", "="))
			{
				ret.language = parseLanguageTagRFC1766;
				break;
			}
			else
			{
				auto p = parseXParam(forward!errorInfo, ret.name);
				ret.params ~= p;
			}
		}

		if (!tokens.matchTokens(":"))
		{
			error(forward!errorInfo, ret.name, "Expected value following xprop");
			return ret;
		}
		ret.value = parseText(errorInfo, ret.name);
		if (!tokens.matchCRLF)
			error(forward!errorInfo, ret.name, "Expected CRLF following xprop value");
		return ret;
	}

	XParam parseXParam(T...)(auto ref T errorInfo)
	{
		XParam ret;
		ret.name = parseXName;
		if (!tokens.matchTokens("="))
		{
			error(errorInfo, "Started xparam but didn't get a value");
			return ret;
		}
		do
		{
			ret.values ~= parseParamValue(errorInfo);
		} while (tokens.matchTokens(","));
		return ret;
	}

	string parseXName()
	{
		size_t len = tokens.data.length;
		foreach (i, c; tokens.data)
		{
			if (!isAlpha(c) && !isDigit(c) && c != '-')
			{
				len = i;
				break;
			}
		}
		string ret = tokens.data[0 .. len].idup;
		tokens.data = tokens.data[len .. $];
		return ret;
	}

	string parseParamValue(T...)(auto ref T errorInfo)
	{
		if (tokens.data.startsWith('"'))
			return parseQuotedString(errorInfo);
		else
			return parseParamText;
	}

	string parseQuotedString(T...)(auto ref T errorInfo)
	{
		if (tokens.data.startsWith('"'))
		{
			tokens.data = tokens.data[1 .. $];
			size_t len = tokens.data.length;
			foreach (i, c; tokens.data)
			{
				if (!c.isQSafeChar)
				{
					len = i;
					break;
				}
			}
			string ret = tokens.data[0 .. len].idup;
			tokens.data = tokens.data[len .. $];
			if (!tokens.matchTokens("\""))
				error(errorInfo, "Expected closing quote or got invalid character in quoted string");
			return ret;
		}
		else
			return null;
	}

	string parseParamText()
	{
		size_t len = tokens.data.length;
		foreach (i, c; tokens.data)
		{
			if (!c.isSafeChar)
			{
				len = i;
				break;
			}
		}
		string ret = tokens.data[0 .. len].toUpper.idup;
		tokens.data = tokens.data[len .. $];
		return ret;
	}

	string parseText(T...)(auto ref T errorInfo)
	{
		import std.utf : codeLength, byDchar;

		auto ret = appender!string;
		bool escape = false;
		size_t i, len;
		foreach (c; tokens.data.byDchar)
		{
			auto cl = codeLength!char(c);
			scope (exit)
				i += cl;

			if (escape)
			{
				escape = false;
				if (c == '\\' || c == ';' || c == ',')
					ret.put(c);
				else if (c == 'N' || c == 'n')
					ret.put('\n');
				else
				{
					error(errorInfo, "Invalid escape sequence '\\" ~ c.to!string ~ "' in text");
					if (!(c.isTSafeChar || c == ':' || c == '"'))
						break;
				}
				len = i + cl;
			}
			else
			{
				if (c == '\\')
				{
					escape = true;
				}
				else if (!(c.isTSafeChar || c == ':' || c == '"'))
				{
					break;
				}
				else
				{
					len = i + cl;
					ret.put(c);
				}
			}
		}
		if (escape)
			error(errorInfo, "Invalid escape sequence \\ following EOF in text");
		tokens.data = tokens.data[len .. $];
		return ret.data;
	}

	template parseTypedDateTime(T...)
	{
		SumType!T parseTypedDateTime(Entry)(string prop, ref Entry e)
		{
			auto ret = parseTypedDateTimeArray!T(prop, e);
			if (ret.length == 0)
				return typeof(return).init;
			else if (ret.length == 1)
				return ret[0];
			else
			{
				error(e, prop, text("Time property ", prop, " does not allow more than one value, but got ", ret));
				return typeof(return).init;
			}
		}
	}

	template parseTypedDateTimeArray(T...)
	{
		SumType!T[] parseTypedDateTimeArray(Entry)(string prop, ref Entry e)
		{
			int requiredType = -1;
			ParamLoop: while (tokens.matchTokens(";"))
			{
				if (tokens.matchTokens("VALUE", "="))
				{
					if (requiredType != -1)
					{
						error(e, prop, text("Time property ", prop, " specifies value type more than once, which is illegal"));
						return null;
					}
					static foreach (i, SubType; T)
					{
						static if (!is(SubType == typeof(null)))
							if (tokens.matchTokens(dateTimeValueParam!SubType))
							{
								requiredType = i;
								continue ParamLoop;
							}
					}
					error(e, prop, text("Unexpected value type ", tokens.peekUntil(":"), " for property ",
						prop, " (allowed: ", [staticMap!(dateTimeValueParam, T)], ")"));
					return null;
				}
				else
				{
					parseXParam(e, prop);
				}
			}
			if (!tokens.matchTokens(":"))
			{
				error(e, prop, text("Expected value following ", prop));
				return null;
			}
			scope (exit)
			{
				if (!tokens.matchCRLF())
					error(e, prop, text("Missing CRLF after ", prop, " value"));
			}

			typeof(return) ret;
			bool matched = false;

			static foreach (i, SubType; T)
			{
				static if (!is(SubType == typeof(null)))
					if ((requiredType == -1 && !matched) || requiredType == i)
					{
						do
						{
							auto recover = tokens.data;
							auto p = requiredType == -1
								? typeof(ret[0])(mixin("parse" ~ SubType.stringof ~ "(null)"))
								: typeof(ret[0])(mixin("parse" ~ SubType.stringof ~ "(e, prop)"));
							if (requiredType == -1 && p == typeof(p).init)
							{
								tokens.data = recover;
								break;
							}
							matched = true;
							ret ~= p;
						} while (tokens.matchTokens(","));
					}
			}

			if (!matched)
			{
				static foreach (i, SubType; T)
				{{
					static if (!is(SubType == typeof(null)))
					{
						auto res = mixin("parse" ~ SubType.stringof ~ "(null)");
						assert(res == typeof(res).init, "previously all parse failed, but now succeeded");
					}
				}}
			}

			return ret;
		}
	}

	SysTime parseDateTime(T...)(auto ref T errorInfo)
	{
		enum noErrorTrack = T.length == 1 && is(T[0] == typeof(null));
		enum dateTimeFormatLength = "YYYYMMDDTHHMMSS".length;
		if (tokens.data.length < dateTimeFormatLength)
		{
			static if (!noErrorTrack)
				error(errorInfo, "datetime is not long enough");
			return SysTime.init;
		}
		auto dateTime = tokens.data[0 .. dateTimeFormatLength];
		if (dateTime[8] != 'T' || !dateTime[0 .. 8].all!isDigit || !dateTime[9 .. $].all!isDigit)
		{
			static if (!noErrorTrack)
				error(errorInfo, "datetime is not in correct format: must be 15 digits and optionally 'Z' for UTC");
			return SysTime.init;
		}
		bool utc = tokens.data.length >= dateTimeFormatLength + 1
			&& tokens.data[dateTimeFormatLength] == 'Z';
		tokens.data = tokens.data[dateTimeFormatLength + (utc ? 1 : 0) .. $];
		auto year = dateTime[0 .. 4];
		auto month = dateTime[4 .. 6];
		auto day = dateTime[6 .. 8];
		auto hour = dateTime[9 .. 11];
		auto minute = dateTime[11 .. 13];
		auto second = dateTime[13 .. 15];
		if (second == "60")
			second = "59"; // no leap second support

		try
		{
			return SysTime(DateTime(year.to!int, month.to!int, day.to!int,
					hour.to!int, minute.to!int, second.to!int), utc ? UTC() : null);
		}
		catch (Exception e)
		{
			static if (!noErrorTrack)
				error(errorInfo, "datetime is not in correct format: " ~ e.msg);
			return SysTime.init;
		}
	}

	private alias parseSysTime = parseDateTime;

	Date parseDate(T...)(auto ref T errorInfo)
	{
		enum noErrorTrack = T.length == 1 && is(T[0] == typeof(null));
		enum dateFormatLength = "YYYYMMDD".length;
		if (tokens.data.length < dateFormatLength)
		{
			static if (!noErrorTrack)
				error(errorInfo, "Date value not long enough");
			return Date.init;
		}
		auto date = tokens.data[0 .. dateFormatLength];
		if (!date[0 .. 8].all!isDigit)
		{
			static if (!noErrorTrack)
				error(errorInfo, "date is not in correct format: must be 8 digits");
			return Date.init;
		}
		tokens.data = tokens.data[dateFormatLength .. $];
		auto year = date[0 .. 4];
		auto month = date[4 .. 6];
		auto day = date[6 .. 8];

		try
		{
			return Date(year.to!int, month.to!int, day.to!int);
		}
		catch (Exception e)
		{
			static if (!noErrorTrack)
				error(errorInfo, "date is not in correct format: " ~ e.msg);
			return Date.init;
		}
	}

	Duration parseDuration(T...)(auto ref T errorInfo)
	{
		// TODO: this isn't according to spec, but parses everything the spec allows (as long as it fits ulong)
		if (!tokens.data.startsWith("P", "-P", "+P"))
			return Duration.init;
		auto negative = tokens.data[0] == '-';
		if (negative || tokens.data[0] == '+')
			tokens.data = tokens.data[2 .. $];
		else
			tokens.data = tokens.data[1 .. $];

		Duration dur;
		while (true)
		{
			if (tokens.data.startsWith('T'))
				tokens.data = tokens.data[1 .. $];
			if (!tokens.data.length || !isDigit(tokens.data[0]))
				break;
			auto num = tokens.data.parse!ulong;
			if (!tokens.data.length)
			{
				error(errorInfo, "Invalid EOF in duration string");
				break;
			}
			switch (tokens.data[0])
			{
			case 'W':
				dur += weeks(cast(long) num);
				break;
			case 'H':
				dur += hours(cast(long) num);
				break;
			case 'M':
				dur += minutes(cast(long) num);
				break;
			case 'S':
				dur += seconds(cast(long) num);
				break;
			case 'D':
				dur += days(cast(long) num);
				break;
			default:
				error(errorInfo, "Invalid duration suffix in string");
				break;
			}
		}

		return negative ? -dur : dur;
	}

	Period parsePeriod(T...)(auto ref T errorInfo)
	{
		auto start = parseDateTime(forward!errorInfo);
		if (start == SysTime.init)
		{
			error(errorInfo, "period is missing valid start date");
			return Period.init;
		}
		if (!tokens.matchTokens("/"))
		{
			error(errorInfo, "period is missing slash to determine next value");
			return Period.init;
		}

		if (!tokens.data.length)
		{
			error(errorInfo, "period is missing duration or end date");
			return Period.init;
		}

		Period ret;
		switch (tokens.data[0])
		{
		case '+':
		case '-':
		case 'P':
			auto d = parseDuration(forward!errorInfo);
			ret = Period(start, start + d);
			break;
		default:
			ret = Period(start, parseDateTime(forward!errorInfo));
			break;
		}

		if (ret.start == SysTime.init || ret.end == SysTime.init
			|| ret.start > ret.end)
		{
			error(errorInfo, "period is malformed, start must be valid date before end");
			return Period.init;
		}

		return ret;
	}

	LanguageTag parseLanguageTagRFC1766()
	{
		LanguageTag ret;
		bool first = true;
		do
		{
			size_t len = tokens.data.length;
			foreach (i, c; tokens.data)
			{
				if (i >= 8 || !isAlpha(c))
				{
					len = i;
					break;
				}
			}
			if (first)
				ret.primaryTag = tokens.data[0 .. len].idup;
			else
				ret.subTags ~= tokens.data[0 .. len].idup;

			tokens.data = tokens.data[len .. $];
			first = false;
		}
		while (tokens.matchTokens("-"));
		return ret;
	}
}

/// Simple parsing function that takes in iCal file content and returns a
/// VCalendar object.
VCalendar parseCalendar(in char[] data, ParseErrorCallback onError = null)
{
	return ICalendarParser(Tokenizer(data), onError).parseCalendar;
}

///
unittest
{
	auto cal = parseCalendar(`
BEGIN:VCALENDAR
PRODID:TestKalender
VERSION:2.0
CALSCALE:GREGORIAN
BEGIN:VEVENT
DTSTAMP:20181212T212342Z
UID:20181231T123857Z-uidGen@webfreak.org
DTSTART:20181207T070000Z
DTEND:20181207T083000Z
SUMMARY:Prüfung: test
 
LOCATION:Raum1\, Raum2
END:VEVENT
BEGIN:VEVENT
DTSTAMP:20220501T145021Z
UID:20220503T145021Z-uidGen@webfreak.org
DTSTART;VALUE=DATE:20220503
DTEND;VALUE=DATE:20220503
SUMMARY:Full day event
END:VEVENT
END:VCALENDAR
	`.splitLines.join("\r\n"), (ref partial, error) {
		assert(false, error.to!string ~ "\n\nPartial result: " ~ partial.to!string);
	});

	assert(cal.productId == "TestKalender");
	assert(cal.specVersion == "2.0");
	assert(cal.scale == "GREGORIAN");
	assert(cal.entries.length == 2);

	auto event0 = cal.entries[0].tryMatch!((VEvent v) => v);
	assert(event0.dtStamp == SysTime(DateTime(2018, 12, 12, 21, 23, 42), UTC()));
	assert(event0.uid == "20181231T123857Z-uidGen@webfreak.org");
	assert(event0.dtStart.tryMatch!((SysTime v) => v) == SysTime(DateTime(2018, 12, 7, 7, 0, 0), UTC()));
	assert(event0.dtEnd.tryMatch!((SysTime v) => v) == SysTime(DateTime(2018, 12, 7, 8, 30, 0), UTC()));
	assert(event0.summary == "Prüfung: test", [event0.summary].to!string);
	assert(event0.location == "Raum1, Raum2");

	cal.entries[1].tryMatch!((VEvent event) {
		assert(event.dtStamp == SysTime(DateTime(2022, 5, 1, 14, 50, 21), UTC()));
		assert(event.uid == "20220503T145021Z-uidGen@webfreak.org");
		assert(event.dtStart.tryMatch!((Date v) => v) == Date(2022, 5, 3));
		assert(event.dtEnd.tryMatch!((Date v) => v) == Date(2022, 5, 3));
		assert(event.summary == "Full day event");
	});
}

enum dateTimeValueParam(T : typeof(null)) = "";
enum dateTimeValueParam(T : SysTime) = "DATE-TIME";
enum dateTimeValueParam(T : Date) = "DATE";
enum dateTimeValueParam(T : Duration) = "DURATION";
enum dateTimeValueParam(T : Period) = "PERIOD";

unittest
{
	import std.file;
	import std.stdio;

	auto cal = parseCalendar(readText("testdata/ms1.ics"), (ref partial, error) {
		if (!error.message.endsWith("not implemented"))
			assert(false, error.to!string ~ "\n\nPartial result: " ~ partial.to!string);
		else
			writeln(error.message);
	});

	assert(cal.method == "REQUEST");
	assert(cal.productId == "Microsoft Exchange Server 2010");
	assert(cal.specVersion == "2.0");
	assert(cal.entries.length == 2);

	// timezone, not yet implemented
	assert(cal.entries[0].tryMatch!((typeof(null) _) => true));

	VEvent event = cal.entries[1].tryMatch!((VEvent e) => e);
	assert(event.organizer == "MAILTO:example@test.de");
	assert(event.attendees.length == 1);
	assert(event.attendees[0].address == "MAILTO:test1234@webfreak.org");
	assert(event.description == "\n");
	assert(event.uid == "0123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678942");
	assert(event.summary == "test: An anderem Ort tätig");
	// timezones not yet implemented, so this is localtime
	assert(event.dtStart.tryMatch!((SysTime v) => v) == SysTime(DateTime(2022, 5, 3, 17, 30, 0)));
	assert(event.dtEnd.tryMatch!((SysTime v) => v) == SysTime(DateTime(2022, 5, 3, 18, 30, 0)));
	assert(event.classification == "PUBLIC");
	assert(event.priority == 5);
	assert(event.dtStamp == SysTime(DateTime(2022, 5, 3, 15, 8, 15), UTC()));
	assert(event.timeTransparency == TimeTransparency.opaque);
	assert(event.status == EventStatus.confirmed);
	assert(event.sequenceNumber == 0);
	assert(event.location == "");
	assert(event.xProps.length == 10);
	assert(event.xProps[0] == XProp("X-MICROSOFT-CDO-APPT-SEQUENCE", "0"));
	assert(event.xProps[1] == XProp("X-MICROSOFT-CDO-OWNERAPPTID", "2120555927"));
	assert(event.xProps[2] == XProp("X-MICROSOFT-CDO-BUSYSTATUS", "TENTATIVE"));
	assert(event.xProps[3] == XProp("X-MICROSOFT-CDO-INTENDEDSTATUS", "FREE"));
	assert(event.xProps[4] == XProp("X-MICROSOFT-CDO-ALLDAYEVENT", "FALSE"));
	assert(event.xProps[5] == XProp("X-MICROSOFT-CDO-IMPORTANCE", "1"));
	assert(event.xProps[6] == XProp("X-MICROSOFT-CDO-INSTTYPE", "0"));
	assert(event.xProps[7] == XProp("X-MICROSOFT-DONOTFORWARDMEETING", "FALSE"));
	assert(event.xProps[8] == XProp("X-MICROSOFT-DISALLOW-COUNTER", "FALSE"));
	assert(event.xProps[9] == XProp("IANA-EXT", "TRUE"));
}

version(none) unittest
{
	import std.file;

	auto cal = parseCalendar(readText("test.ics"), (ref partial, error) {
		assert(false, error.to!string ~ "\n\nPartial result: " ~ partial.to!string);
	});

	// import std.stdio;
	// writeln(cal);
}
