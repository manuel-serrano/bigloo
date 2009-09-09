#!/bin/sh

kdialog --msgbox "Please run '$*' on the target and capture its output and return status.\nEx: $* > /tmp/output; echo Return-status: \$?"

EXIT_STATUS=`kdialog --title "Return status" \
                     --inputbox "Please enter the return status" "0"`

if [ ! $? = 0 ]; then
    kdialog --sorry "Cancel is not allowed. Assumed return status 0."
    EXIT_STATUS="0"
fi

kdialog --textinputbox "Please enter output (or cancel for output-file)"

if [ ! $? = 0 ]; then
    OUTPUT_FILE=`kdialog --getopenfilename .`
    cat $OUTPUT_FILE
fi

exit $EXIT_STATUS
