import time

# <here goes the Time class with its methods>
""" Name: Francesco Pavlovic
 Study: BSc Informatica
 UvAnetID: 13782118
"""

HOURS_IN_DAY = 24
MINUTES_IN_HOUR = 60
SECONDS_IN_MINUTE = 60


class Time:
    """ Represents a time of day."""

    def __init__(self, hours, minutes, seconds):
        """ Initialises a Time object with integers 'hours', 'minutes' and
        'seconds'.
        >>> t = Time(18, 30, 0)
        """
        self.set_time(hours, minutes, seconds)

    def __repr__(self):
        """ Returns the string representation of a Time object.
        >>> print( Time(8,5,30) )
        08:05:30
        """
        hours = str(self.hours)
        minutes = str(self.minutes)
        seconds = str(self.seconds)

        return (str(hours.zfill(2)) + ":" + str(minutes.zfill(2)) + ":" +
                str(seconds.zfill(2)))

    def get_hours(self):
        """ Returns the hours of the Time object.
        >>> Time(23,0,0).get_hours()
        23
        """
        return print(self.hours)

    def get_minutes(self):
        """ Returns the minutes of the Time object.
        >>> Time(0,59,0).get_minutes()
        59
        """
        return print(self.minutes)

    def get_seconds(self):
        """ Returns the seconds of the Time object.
        >>> Time(0,0,59).get_seconds()
        59
        """
        return print(self.seconds)

    def set_time(self, hours, minutes, seconds):
        """ Sets the time of the Time object to 'hours', 'minutes',
        and 'seconds' making sure the values are in valid range:
        hours:   [0, HOURS_IN_DAY)
        minutes: [0, MINUTES_IN_HOUR)
        seconds: [0, SECONDS_IN_MINUTE)
        >>> time = Time(0, 0, 0)
        >>> time.set_time(0, 0, 90)
        >>> print(time)
        00:01:30
        >>> time.set_time(0, 0, 3600)
        >>> print(time)
        01:00:00
        >>> time.set_time(0, 0, -1)
        >>> print(time)
        23:59:59
        >>> time.set_time(10, -121, 0)
        >>> print(time)
        07:59:00
        >>> time.set_time(-50, 0, 0)
        >>> print(time)
        22:00:00
        >>> print(Time(10, -120, -150)) # __init__() test
        07:57:30
        """
        seconds_t = divmod(seconds, 60)
        if seconds_t[1] < 0:
            minutes -= 1
            hours -= 1
            seconds %= 60
        else:
            minutes += seconds_t[0]

        minutes_t = divmod(minutes, 60)
        if minutes_t[1] < 0:
            hours -= 1
            minutes %= 60
        else:
            hours += minutes_t[0]

        hours %= 24

        self.hours = hours
        self.minutes = minutes_t[1]
        self.seconds = seconds_t[1]

    def get_total_seconds(self):
        """ Returns the number of seconds since time 00:00:00.
        >>> Time(0,0,1).get_total_seconds()
        1
        >>> Time(0,1,0).get_total_seconds()
        60
        >>> Time(1,0,0).get_total_seconds()
        3600
        >>> Time(13,30,5).get_total_seconds()
        48605
        """
        return self.seconds + (self.minutes * 60) + (self.hours * 3600)

    def __add__(self, other):
        """ Returns a valid Time objects which is Time objects
        'other' added to 'self'.
        >>> print(Time(0,0,0) + Time(1,2,3))
        01:02:03
        >>> print(Time(13,30,0) + Time(1,46,-45))
        15:15:15
        """
        self.seconds += other.seconds
        if self.seconds >= 60 or self.seconds < 0:
            seconds_t = divmod(self.seconds, 60)
            self.seconds = seconds_t[1]
            self.minutes += seconds_t[0]

        self.minutes += other.minutes
        if self.minutes >= 60 or self.minutes < 0:
            minutes_t = divmod(self.minutes, 60)
            self.minutes = minutes_t[1]
            self.hours += minutes_t[0]

        self.hours += other.hours
        if self.hours >= 24 or self.hours < 0:
            self.hours %= 24

        return Time(self.hours, self.minutes, self.seconds)

    def __sub__(self, other):
        """ Returns a valid Time objects which is Time objects
        'other' substracted from 'self'.
        >>> print(Time(10,10,10) - Time(1,2,3))
        09:08:07
        >>> print(Time(10,0,0) - Time(1,50,600))
        08:00:00
        """
        self.seconds -= other.seconds
        if self.seconds < 0:
            self.minutes -= 1
            self.seconds += 60

        self.minutes -= other.minutes
        if self.minutes < 0:
            self.hours -= 1
            self.minutes += 60

        self.hours -= other.hours

        return Time(self.hours, self.minutes, self.seconds)


def get_current_hours_minutes_seconds():
    """ Returns the current (hours, minutes, seconds) as a tuple. """
    t = time.localtime()
    return (t.tm_hour, t.tm_min, t.tm_sec)


def now():
    """ Returns the current time as Time object. """
    return Time(*get_current_hours_minutes_seconds())


class Event:
    """ Represents an event that happens at a certain time."""

    def __init__(self, time, description):
        """ Initialises an Event object with a 'time' object of type Time and a
        'description' of type str.
        >>> event = Event(Time(18, 30, 0), "dinner")
        """
        self.time = time
        self.description = description

    def __repr__(self):
        """ Returns the string representation of an Event object.
        >>> print( Event(Time(18, 30, 0), "dinner") )
        18:30:00 dinner
        """
        return str(self.time) + " " + str(self.description)

    def get_time(self):
        """ Returns the time of an Event object.
        >>> print( Event(Time(18, 30, 0), "dinner").get_time() )
        18:30:00
        """
        return str(self.time)

    def get_description(self):
        """ Returns the description of an Event object.
        >>> print( Event(Time(18, 30, 0), "dinner").get_description() )
        dinner
        """
        return str(self.description)


class AlarmClock:
    """ Represents an alarm clock that can handle events. """

    def __init__(self):
        """ Initialises an AlarmClock object with an empty list of events.
        >>> alarm_clock = AlarmClock()
        """
        self.event_l = []

    def add_event(self, event):
        """ Adds an 'event' to this AlarmClock object, it doesn't return anything.
        >>> alarm_clock = AlarmClock()
        >>> event = Event(Time(18, 30, 0), "dinner")
        >>> alarm_clock.add_event(event)
        """
        self.event_l.append(event)


    def __repr__(self):
        """ Returns a string representation of the AlarmClock object. 
        >>> alarm_clock = AlarmClock()
        >>> event = Event(Time(18, 30, 0), "dinner")
        >>> alarm_clock.add_event(event)
        >>> s = str(alarm_clock)
        >>> "18:30:00" in s
        True
        >>> "dinner" in s
        True
        >>> "breakfast" in s
        False
        """

        return str(self.event_l)
