package org.lawrencebower.docgen.web_model.view.view_factory.tsv_factory;

import org.lawrencebower.docgen.web_model.view.view_factory.factory.BusinessFactory;
import org.lawrencebower.docgen.web_model.view.view_factory.tsv_factory.parser.DataRow;
import org.lawrencebower.docgen.web_model.view.view_factory.tsv_factory.parser.DataSet;
import org.lawrencebower.docgen.web_model.view.view_factory.tsv_factory.parser.TSVReader;
import org.springframework.beans.factory.annotation.Autowired;

import java.util.HashMap;
import java.util.Map;

public class BusinessFactoryTsvImpl implements BusinessFactory {

    @Autowired
    private TSVReader tsvReader;

    private String businessTSVFile;

    private Map<String, String> business;

    public void setBusinessTSVFile(String businessTSVFile) {
        this.businessTSVFile = businessTSVFile;
    }

    private void initBusiness(){

        DataSet dataSet = tsvReader.readDataSetAsFile(businessTSVFile);

        business = new HashMap<>();

        for (DataRow dataRow : dataSet.getRows()) {
            String contactId = dataRow.getString(0);
            String businessId = dataRow.getString(1);

            business.put(contactId, businessId);
        }
    }

    @Override
    public Map<String, String> getBusinesses() {
        return business;
    }
}
