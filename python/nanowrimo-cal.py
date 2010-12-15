#!/usr/bin/env python

import icalendar
import uuid
from datetime import datetime, date

def word_targets():
    for i in range(30):
        yield ((i+1), (i+1) * 1667)

def main():
    cal = icalendar.Calendar()
    cal.add('prodid', '-//NaNoWriMo Calendar//mxm.dk//')
    cal.add('version', '2.0')
    cal.add('summary', "NaNoWriMo Calendar")
    today=date.today()
    targets = [(date(today.year,11,day), target) for (day,target) in word_targets()]
    print targets
    for (thedate, target) in targets:
        event = icalendar.Event()
        event.add('summary', 'Target: '+str(target))
        event.add('dtstart', thedate)
        event.add('dtend', thedate)
        event.add('dtstamp', thedate)
        event.add('rrule', dict(freq='yearly', interval=1))
        event.add('sequence', 0)
        event.add('uuid', uuid.uuid4())
        cal.add_component(event)

    f = open("/Users/will/Desktop/my-nanowrimo.ics", 'wb')
    f.write(cal.as_string())
    f.close()

if __name__ == '__main__':
    main()
