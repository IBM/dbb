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
import com.ibm.team.dbb.build.report.records.Record;

/**
 * Sample build report record to store Git hash information for 
 * each file
 *
 */
public class GitHashRecord extends Record
{
    public static final String PROP_FILE_PATH = "filePath";
    public static final String PROP_COMMIT_HASH_VALUE = "commitHashValue";
    
    private String filePath;
    private String commitHashValue;
    
    /**
     * Constructor
     */
    public GitHashRecord(String id)
    {
        super(id, GitRecordFactory.TYPE_GIT_HASH);
    }

    /**
     * Return the path of the file
     * @return the path of the file
     */
    public String getFilePath()
    {
        return filePath;
    }

    /**
     * Set the path of the file
     * @param filePath the path of the file
     */
    public void setFilePath(String filePath)
    {
        this.filePath = filePath;
    }

    /**
     * Return the hash value of the file
     * @return the hash value of the file
     */
    public String getCommitHashValue()
    {
        return commitHashValue;
    }

    /**
     * Set the hash value of the file
     * @param commitHashValue the hash value of the file
     */
    public void setCommitHashValue(String commitHashValue)
    {
        this.commitHashValue = commitHashValue;
    }

    @Override
    public JSONObject toJSON()
    {
        JSONObject jsonObj = super.toJSON();
        jsonObj.put(PROP_FILE_PATH, filePath);
        jsonObj.put(PROP_COMMIT_HASH_VALUE, commitHashValue);
        return jsonObj;
    }

    @Override
    public Record parse(JSONObject jsonObj)
    {
        Record record = super.parse(jsonObj);
        filePath = (String)jsonObj.get(PROP_FILE_PATH);
        commitHashValue = (String)jsonObj.get(PROP_COMMIT_HASH_VALUE);
        return record;
    }

    
}
