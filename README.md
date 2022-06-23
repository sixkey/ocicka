# Syntax

every <interval> <action> 
after <interval> <action>
at <time> <action>
( <action>, <action>, ..., <action> )
ping <message>
choose 

every 1h ( ping "stand", after 15m "sit" )
every 1h ( 
    ping ( choose "drink" "look into the distance" "roll your eyes" ) 
    after 20m ping ( choose )
)
every 2h ( ping 

