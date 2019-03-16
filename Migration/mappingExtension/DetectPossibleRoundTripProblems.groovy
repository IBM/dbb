import com.ibm.jzos.*

class DetectPossibleRoundTripProblems
{
    def CHAR_NL = 0x15
    def CHAR_CR = 0x0D
    def CHAR_LF = 0x25
    def CHAR_SHIFT_IN = 0x0F
    def CHAR_SHIFT_OUT = 0x0E

    /**
     * Detect whether a member contains record that contains a line separator or an empty Shift-In Shift-Out
     * @param dataset the data set contains the member to test
     * @param member the member to test
     * @return an array contains error code 0 = No Errors round, 1 = Contains 1 of the errors described in
     * the above description, 2 = Internal Error; and the actual error message.
     */
    def call(String dataset, String member)
    {
        def fullyQualifiedDsn = constructDatasetForZFileOperation(dataset, member)        
        try
        {            
            def reader = RecordReader.newReader(fullyQualifiedDsn, ZFileConstants.FLAG_DISP_SHR)
            byte[] buf = new byte[reader.getLrecl()]
            int line = 0                                
            while (reader.read(buf) >= 0)
            {
                line++
                def prevIndex = -1
                def foundEmptyShiftOutShiftIn = false
                
                /* Find NL, CR, LF in a record */
                def found = buf.find { it == CHAR_NL || it == CHAR_CR || it == CHAR_LF }
                if (found)
                    return [1, "Line $line contains a line separator 0x${sprintf('%02X',found)}"]
                    
                /* Find an empty Shift In and Shift Out */                    
                buf.eachWithIndex {nextByte, nextIndex ->
                    if (nextByte == CHAR_SHIFT_IN)                    
                        prevIndex = nextIndex                    
                    else if (nextByte == CHAR_SHIFT_OUT && prevIndex != -1 && prevIndex == (nextIndex-1))
                        foundEmptyShiftOutShiftIn = true
                }                  
                if (foundEmptyShiftOutShiftIn)
                    return [1, "Line $line contains empty Shift In and Shift Out"]
            }
        }
        catch (IOException e)
        {
            return [2, "Internal error ${e.message}"]
        }
        return [0, "SUCCESS"]
    }
    
    /**
     * All zFile operations require dataset and member in certain format
     * @param dataset the data set
     * @param member the member in the data set
     * @return a formatted string contains the data set and member for zFile
     * operation
     */
    def constructDatasetForZFileOperation(String dataset, String member)
    {
        return "//'${dataset}($member)'"
    }
}
