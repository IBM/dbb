/*******************************************************************************
 * Licensed Materials - Property of IBM
 * (c) Copyright IBM Corporation 2018. All Rights Reserved.
 * 
 * Note to U.S. Government Users Restricted Rights:  
 * Use, duplication or disclosure restricted by GSA ADP Schedule 
 * Contract with IBM Corp. 
 *******************************************************************************/
package com.ibm.team.dbb.build.ext.git.report;

import com.ibm.json.java.JSONObject;
import com.ibm.team.dbb.build.report.records.AbstractRecordFactory;
import com.ibm.team.dbb.build.report.records.IRecordFactory;
import com.ibm.team.dbb.build.report.records.Record;

/**
 * The factory to create GIT record.
 *
 */
public class GitRecordFactory extends AbstractRecordFactory implements IRecordFactory
{
    public static final String TYPE_GIT_HASH = "GIT_HASH";

    
    /**
     * Create a record with the specified ID and type.
     */
    @Override
    public Record createRecord(String id, String type)
    {
        if (TYPE_GIT_HASH.equals(type))
            return new GitHashRecord(id);
        return null;
    }
    
    @Override
    protected Record doParseRecord(JSONObject jsonObj, String id, String type)
    {
        Record record = createRecord(id, type);
        if (record != null)
            record.parse(jsonObj);
        return record;
    }

    @Override
    public String[] getAllSupportedTypes()
    {
        return new String[] {TYPE_GIT_HASH};
    }

    
}
