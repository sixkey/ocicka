# Syntax

```
<action> = every <interval> <action>             # action is executed every interval
         | after <interval> <action>             # action is executed after interval
         | ( <action>, <action>, ..., <action> ) # all actions are run simultaneously 
         | ping <message>                        # notify-send the message 
<interval> = <positive_integer><unit>            
<unit>     = s                                   # seconds
           | m                                   # minutes
           | h                                   # hours
<message>  = "lorem ipsum"                       # normal text message
           | oneof [ "text1", ..., "textn" ]     # chooses one of the messages at random
           | oneof <variable>                    # chooses one of the messages from variable
<message box> =               # each line is one text
    -- <variable>
    text1
    text2
    ...
    textn 
    --
<variable> = ? string from alphaNum starting with a letter ?
```

# Example

Example can be found in `example.oc`. To run ocicka simply run `runghc src/main.hs examples/example.oc`.
