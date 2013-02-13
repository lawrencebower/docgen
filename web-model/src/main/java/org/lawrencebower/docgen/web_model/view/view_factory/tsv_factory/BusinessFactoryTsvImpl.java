package org.lawrencebower.docgen.web_model.view.view_factory.tsv_factory;

import org.lawrencebower.docgen.core.exception.DocGenException;
import org.lawrencebower.docgen.web_model.view.view_factory.factory.BusinessFactory;
import org.lawrencebower.docgen.web_model.view.view_factory.tsv_factory.parser.DataRow;
import org.lawrencebower.docgen.web_model.view.view_factory.tsv_factory.parser.DataSet;
import org.lawrencebower.docgen.web_model.view.view_factory.tsv_factory.parser.TSVReader;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.core.io.Resource;

import java.util.HashMap;
import java.util.Map;

public class BusinessFactoryTsvImpl implements BusinessFactory {

    @Autowired
    private TSVReader tsvReader;

    @javax.annotation.Resource
    @Qualifier("businessTSVFile")
    private Resource businessTSVFile;

    private Map<String, String> businesss;

    private void initBusiness(){

        DataSet dataSet = tsvReader.readDataSetAsFile(businessTSVFile);

        businesss = new HashMap<>();

        for (DataRow dataRow : dataSet.getRows()) {
            String contactId = dataRow.getString(0);
            String businessId = dataRow.getString(1);

            businesss.put(contactId, businessId);
        }
    }

    @Override
    public void reloadBusiness(){
        initBusiness();
    }

    @Override
    public Map<String, String> getBusinesses() {
        return businesss;
    }

    @Override
    public boolean containsContactId(String contactId) {
        return businesss.containsKey(contactId);
    }

    @Override
    public String getMappedBusiness(String contactId) {

        if(!containsContactId(contactId)){
            String message = String.format("No business mapped for contactId '%s'", contactId);
            throw new DocGenException(message);
        }

        return businesss.get(contactId);
    }
}
