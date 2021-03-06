        ??  ??                    ,   T E X T   D E M 1       0           Memo 1                     

SCALING DEMO

THE FORM FOLDER:
Docking a Form puts it on the
end of the list not on top of
the currently open form. Dragging
the last form off cycles back
to the beginning of the list
not to the preceding form.
This does not matter for a few
forms but is not good when many
forms are docked. Right click
on one of the tabs and select
MultiLine Tabs.
Only a partial solution I think.

THE FORM STACK does not have
these problems. Forms are kept
in alphabetical order and
minimizing a displayed form by
clicking its button displays the
complete list. The size of the
displayed form can be modified
using the splitters and by
dragging a side. When a different
record is selected in the DbGrid
the Form's caption changes but
it remains where it is in the
list. Clicking the Sink button
sinks the form and any other out
of order forms to their correct
location.

USING THE DEMO

Initially Forms in the Form Folder
are close to Design Size.

Right click a Form for its context
menu.
  'Show Scroller' to see scaleable
   controls scroll
  'Stack' the form: it jumps onto
   the Form Stack.
  'Hide Scroller'

   Drag the form off the Stack by
   its Title Bar
  'Memo 1: no scaling' shows the
   size the Memo would be
   determined by it anchor
   settings. Scaling has done a
   useful job by keeping the size
   of controls in proportion.
  'Design Size' puts the Form at its
   design size and the Memo is
   back in position.
  'Memo 1: do scaling'

Maximize the Form
   Text size increases for Memo 1
   but not disproportionately
  'Memo 1: Text -- no max.' Text is
   scaled in proportion to the
   current width of the Memo.
   Minimize and Maximize the
   Form again to check this
   then do
  'Memo 1: Text Size 13'

With the Form maximized No-
Upscaling has come into play and
the size-position of controls is
determined by anchors. Uncheck
the NoUpscaling checkbox and
Memo 1 is scaled in proportion
to the current size of the Form.
The NoUpscaling facility
combined with size and position
scaling maximizes usage of
available area whether Forms are
larger or smaller than Design Size.

Using the checkboxes the range
of available scaling options can
be examined. A scaling takes
precedence over an anchor setting
if it is present and not inhibited.
Anchoring is different to standard
controls. Setting a standard
control anchor fixes its current
edge location. Setting a scaleable
control anchor at runtime
relocates the control as though
the anchor had been set at design
time. The Design Size of a 
component can be changed at
runtime so any modifications can
be done in relation to the Form as
designed.

Design and scaled size of Memo 1
can be changed with the up downs.
'Enable Memo 2' logs the size and
position of Memo 1 +
[Form size/Form client rect. size]
Text scaling is independent of
other scalings i.e. with scText
on, text is always scaled to the
current width of a control
irrespective of NoUpScaling
and DoScaling properties.
   