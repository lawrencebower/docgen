package org.lawrencebower.docgen.web_model.view.view_factory.tsv_factory;

import org.lawrencebower.docgen.web_model.view.contact.ContactView;
import org.lawrencebower.docgen.web_model.view.view_factory.factory.CustomerFactory;
import org.lawrencebower.docgen.web_model.view.view_factory.tsv_factory.parser.DataRow;
import org.lawrencebower.docgen.web_model.view.view_factory.tsv_factory.parser.DataSet;
import org.lawrencebower.docgen.web_model.view.view_factory.tsv_factory.parser.TSVReader;
import org.springframework.beans.factory.annotation.Autowired;

import java.util.HashMap;
import java.util.Map;

public class CustomerFactoryTsvImpl implements CustomerFactory {

    private String customersTSVFile;

    @Autowired
    private TSVReader tsvReader;
    @Autowired
    private ContactMapper contactMapper;

    private Map<String, ContactView> customers;

    public void setCustomersTSVFile(String customersTSVFile) {
        this.customersTSVFile = customersTSVFile;
    }

    private void initCustomers(){

        DataSet dataSet = tsvReader.readDataSetAsFile(customersTSVFile);

        customers = new HashMap<>();

        for (DataRow dataRow : dataSet.getRows()) {
            ContactView customer = mapCustomerInfo(dataRow);
            String customerId = customer.getContactId();
            customers.put(customerId, customer);
        }
    }

    private ContactView mapCustomerInfo(DataRow dataRow) {
        return contactMapper.mapCustomerInfo(dataRow);
    }

    @Override
    public Map<String, ContactView> getCustomers() {
        return customers;
    }

}
