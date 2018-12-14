# Migration Sample
The following sample shows how to modify the migration Groovy script to add additional functionality.

## Requirement
Detect possible round-trip encoding problems when importing and loading to HFS.  In this sample, we will address 2 issues:
1. New line character in the record.
2. Empty Shift-In and Shift-Out.

## Background
Migration tool assists user to import members in data set to files in HFS.  User can then deliver these files to the Git repository.  Migration does not do any encoding conversion when migrating members to HFS, so if members are encoded in EBCDIC Cp1047, the files in HFS are also encoded in Cp1047.  However files stored in distributed Git repository are usually encoded in UTF-8 specially source files.  When we mention round-trip conversion, we are referring to the process of converting character set from Cp1047 to UTF-8 when committing to Git and converting it back from UTF-8 to Cp1047 when loading to HFS from the Git repository.  There are some situations where this round-trip conversion does not preserve the original content of the source files.  For example:
1.  **New line character in the record**
The record can contain new line separators (0x15=NL; 0D=CR; 0x25=LF).  When committing such files to Git, these new line characters are converted to a brand new line.  Therefore, after the round trip conversion, the loaded members would have additional empty record compare to the original migrated member.

2.  **Empty Shift-In and Shift-Out**
The Shift-In (0x0F) and Shift-Out (0x0E) are often used to embed different character sets in a text file.  For example, embed a double-byte character sets in a source file.  When committing such files to Git, these characters are stripped off and the double-byte character sets are converted.  When loading these files from Git to HFS, the Shift-In and Shift-Out are re-inserted.  In the case of an empty Shift-In and Shift-Out (0F0E), these characters are stripped off but are never re-inserted later.  Again, the content of loaded member would not be exactly the same as the original migrated member.

## Solution
Provide additional pre-process to detect these scenarios and take appropriate actions during migration.

Implementing this pre-process in Groovy is straight forward, see sample DetectPossibleRoundTripProblems.groovy.  

>               /* Find NL, CR, LF in a record */
>               def found = buf.find { it == CHAR_NL || it == CHAR_CR || it == CHAR_LF }
>               if (found)
>                   return [1, "Line $line contains a line separator 0x${sprintf('%02X',found)}"]

and

>              /* Find an empty Shift In and Shift Out */                    
>              buf.eachWithIndex { nextByte, nextIndex ->
>              if (nextByte == CHAR_SHIFT_IN)                    
>                  prevIndex = nextIndex                    
>              else if (nextByte == CHAR_SHIFT_OUT && prevIndex != -1 && prevIndex == (nextIndex-1))
>                  foundEmptyShiftOutShiftIn = true
>              }                  
>              if (foundEmptyShiftOutShiftIn)
>                  return [1, "Line $line contains empty Shift In and Shift Out"]

The above snippets show how we detect new line characters and empty Shift-In and Shift-Out after reading in the record.  In this sample, we return an error code (0=No Failure; 1=Error detected with round-trip encoding) and an error message.  The next step is to incorporate this pre-process into the existing migrate.groovy.  In this example, the action we will take when encountering these scenarios is to continue migrating these members but flagging this file in the **.gitignore** file as 'binary' so that there is no EBCDIC to UTF-8 conversion when committing to Git repository.  

First, adding a reference of this script in migrate.groovy:

>                def detector = new DetectPossibleRoundTripProblems()

Secondly, call this method on each member being migrated:

>                def (rc, msg) = detector(mappingInfo.dataset, mappingInfo.member)

Lastly, check the rc and flagging this file in .gitignore when one of these scenarios are detected:

>                if (rc)
>                    gitAttributeLine = repository.toPath().relativize(hfsFile.toPath()).toFile().path + " binary"

In this sample, we migrate these particular files to the same directory as other files, but you might want to move these files to a particular 'bin' directory, in that case, simply changing the following in migrate.groovy:

>               new CopyToHFS().dataset(mappingInfo.dataset).member(mappingInfo.member).file(new File(mappingInfo.hfsPath)).pdsEncoding(mappingInfo.pdsEncoding).execute();

to

>               def hfsFile = new File(mappingInfo.hfsPath)
>               def newHfsPath = "${hfsFile.parent.absolutePath}/bin/${hfsFile.name}" 
>               new CopyToHFS().dataset(mappingInfo.dataset).member(mappingInfo.member).file(new File(newHfsPath)).pdsEncoding(mappingInfo.pdsEncoding).execute();

## Attachment
Completed code sample is in the attached DetectPossibleRoundTripProblems.groovy and Migrate.groovy.





