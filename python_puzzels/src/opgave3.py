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