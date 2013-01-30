package org.lawrencebower.docgen.web_model.view.view_factory.tsv_factory;

import org.lawrencebower.docgen.core.exception.DocGenException;
import org.lawrencebower.docgen.web_model.view.contact.Contact;
import org.lawrencebower.docgen.web_model.view.contact.ContactView;
import org.lawrencebower.docgen.web_model.view.contact.ContactViewFactory;
import org.lawrencebower.docgen.web_model.view.view_factory.factory.CustomerFactory;
import org.lawrencebower.docgen.web_model.view.view_factory.tsv_factory.parser.DataRow;
import org.lawrencebower.docgen.web_model.view.view_factory.tsv_factory.parser.DataSet;
import org.lawrencebower.docgen.web_model.view.view_factory.tsv_factory.parser.TSVReader;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.core.io.Resource;

import java.util.*;

public class CustomerFactoryTsvImpl implements CustomerFactory {

    @javax.annotation.Resource
    @Qualifier("customersTSVFile")
    private Resource customersTSVFile;

    @Autowired
    private TSVReader tsvReader;
    @Autowired
    private ContactMapper contactMapper;
    @Autowired
    private ContactViewFactory viewFactory;

    private Map<String, Contact> customers;

    private void initCustomers() {

        DataSet dataSet = tsvReader.readDataSetAsFile(customersTSVFile);

        customers = new HashMap<>();

        for (DataRow dataRow : dataSet.getRows()) {
            Contact customer = mapCustomerInfo(dataRow);
            String customerId = customer.getContactId();
            customers.put(customerId, customer);
        }
    }

    private Contact mapCustomerInfo(DataRow dataRow) {
        return contactMapper.mapCustomerInfo(dataRow);
    }

    @Override
    public List<ContactView> getCustomersAsList() {

        List<ContactView> contactViews = new ArrayList<>();
        Collection<Contact> values = customers.values();
        for (Contact contact : values) {
            ContactView contactView = viewFactory.createContactView(contact);
            contactViews.add(contactView);
        }

        return contactViews;
    }

    @Override
    public ContactView getCustomer(String contactId) {

        if (!customers.containsKey(contactId)) {
            String message = String.format("Contact with id '%s' not found?!", contactId);
            throw new DocGenException(message);
        }

        Contact contact = customers.get(contactId);

        return viewFactory.createContactView(contact);
    }

    @Override
    public boolean hasCustomer(String contactId) {
        return customers.containsKey(contactId);
    }

}
