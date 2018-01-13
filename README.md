# timesheet


## Synopsis

Add up timesheet values from a simple text file format (Haskell)


## Description

Add up timesheet hours for days and weeks from a simple text file format.

I needed a simple way to record hours worked during the day and across weeks in a simple text file but allow them to be automatically added together. And preferably without something heavy and graphical like a spreadsheet program.

The program can turn something like this:

    2011-03-12 Sa  10:00-14:00
    2011-03-13 Su  10:00-13:00
    2011-03-14 Mo  09:00-11:00
    2011-03-15 Tu  09:00-11:30  12:15-17:15  18:30-22:00
    2011-03-16 We  09:00-12:00  13:00-13:30

    2011-03-08 Tu  13:30-17:30
    2011-03-09 We  09:00-12:00  13:30-19:00
    2011-03-10 Th  08:30-11:45  13:30-16:30
    2011-03-11 Fr  09:00-10:15


Into this output:

    $ timesheet foo.hours 
    2011-03-12 Sa   4.00  10:00-14:00
    2011-03-13 Su   3.00  10:00-13:00
    2011-03-14 Mo   2.00  09:00-11:00
    2011-03-15 Tu  11.00  09:00-11:30  12:15-17:15  18:30-22:00
    2011-03-16 We   3.50  09:00-12:00  13:00-13:30
    total:         23.5


## Getting source

- Get the source with git: `$ git clone https://github.com/dino-/timesheet.git`
- If you're just looking, [browse the source](https://github.com/dino-/timesheet)

And once you have it, building the usual way:

    $ stack build
    $ stack install


## Contact

Dino Morelli <[dino@ui3.info](mailto:dino@ui3.info)>
