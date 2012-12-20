package org.lawrencebower.docgen.doc_examples.factory;

import org.lawrencebower.docgen.web_logic.view.contact.Contact;
import org.lawrencebower.docgen.web_logic.view.contact.ContactView;
import org.lawrencebower.docgen.web_logic.view.contact.ContactViewFactory;
import org.lawrencebower.docgen.web_logic.view.view_factory.factory.CustomerFactory;
import org.springframework.beans.factory.annotation.Autowired;

import java.util.LinkedHashMap;
import java.util.Map;

public class CustomerFactoryTestImpl implements CustomerFactory {

    @Autowired
    private ContactViewFactory contactViewFactory;

    private Map<String, ContactView> customers = new LinkedHashMap<>();
    private Map<String, ContactView> businesses = new LinkedHashMap<>();

    public static final String CUSTOMER_ID_1 = "Billy Bob's products";
    public static final String CUSTOMER_ID_2 = "David's widgets";


    private void initCustomers() {

        Contact contact1 = new Contact(CUSTOMER_ID_1,
                                       "Billy Bob Bobson",
                                       "Just round the corner",
                                       "198293893839",
                                       "UK");

        Contact contact2 = new Contact(CUSTOMER_ID_2,
                                       "David Davidson",
                                       "miles away",
                                       "38783478347",
                                       "USA");

        ContactView customerView1 = contactViewFactory.createContactView(contact1);
        ContactView customerView2 = contactViewFactory.createContactView(contact2);


        customers.put(customerView1.getName(), customerView1);
        customers.put(customerView2.getName(), customerView2);

        businesses.put(customerView1.getName(), customerView2);
        businesses.put(customerView2.getName(), customerView1);

    }

    @Override
    public Map<String, ContactView> getCustomers() {
        return customers;
    }

    @Override
    public Map<String, ContactView> getBusinesses() {
        return businesses;
    }
}
