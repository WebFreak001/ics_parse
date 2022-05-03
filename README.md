# ics_parse

Parser for .ics files (ical format)

Only supports parsing VEVENT right now.
(no VTODO, VJOURNAL, VFREEBUSY, VTIMEZONE, iana components, custom X-components)

- Does not support non-UTC timezones (timezones are not exposed, but could be and should be by implementing a custom SysTime timezone that's used)
- Does not support VALARM parsing (should be easy to add)

Standards: https://datatracker.ietf.org/doc/html/rfc5545

History:
 * May 2022 - added support for date/datetime value type (DTSTART;VALUE=DATE:20220503) parsing
 * June 2019 - initial version

## Example

```d
import ics_parse;

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
`.splitLines.join("\r\n") /* must be CRLF separated lines */, (ref partial, error) {
	// you get parsing errors which you can choose to ignore, log or abort the program
	// (or do anything else in an event of parsing issue)

	// you can assume partial or full values to be missing, basically completely breaking
	// the event if the property in question is important to you.
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
assert(event0.summary == "Prüfung: test");
assert(event0.location == "Raum1, Raum2");

cal.entries[1].tryMatch!((VEvent event) {
	assert(event.dtStamp == SysTime(DateTime(2022, 5, 1, 14, 50, 21), UTC()));
	assert(event.uid == "20220503T145021Z-uidGen@webfreak.org");
	assert(event.dtStart.tryMatch!((Date v) => v) == Date(2022, 5, 3));
	assert(event.dtEnd.tryMatch!((Date v) => v) == Date(2022, 5, 3));
	assert(event.summary == "Full day event");
});
```
