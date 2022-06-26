# Ocicka

Ocicka is a very simple notification and command planner configured in a simple custom language.

## Syntax

```
<action> = every <interval> <action>             # action is executed every interval
         | after <interval> <action>             # action is executed after interval
         | ( <action>, <action>, ..., <action> ) # all actions are run simultaneously 
         | ping <message>                        # notify-send the message 
         | run <message>                         # run message as a command

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

## Example

```
-- SomeRandomMessages
I am message number 1
I am message number 2
I am message number 3
I am message number 4
--

after 5s every 4s (
    ping "start",
    after 1s ping oneof SomeRandomMessages, 
    after 2s ping "end",
    after 3s run "echo lap"
)

every 1s ping "tick"
```

Example can be found in `example.oc`. To run ocicka simply run `runghc src/main.hs examples/example.oc`.
