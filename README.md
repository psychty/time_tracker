# Time tracker
Hosted R script to track work time - November 2023.  

## Purpose
I created this time tracking R script to read in a google sheets file I have hosted and authenticate R to read in and analyse time given and taken because I notoriously work longer than I should and never track hours (though I do by the same admission work very flexibly around school drop off and their associated appointments).

## The spreadsheet 
This is a google spreadsheet with five columns:

| Date | Hours | Start | End | Given_taken
| --- | --- | --- | --- | --- |
| 01/11/2023 | | 09:30 | 15:50 | Given |
| 01/11/2023 | | 10:10 | 10:50 | Taken |
| 01/11/2023 | | 10:10 | 10:50 | Given |
| 02/11/2023 | | 09:00 | 17:00 | Annual Leave |

## The script 
The script psychty/time_tracker/time_tracker.R reads the spreadsheet and calculates on a weekly basis the number of hours worked, and the difference between usual contracted hours and some assumptions on lunch breaks etc.
