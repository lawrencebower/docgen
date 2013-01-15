package org.lawrencebower.docgen.doc_examples.factory;

import org.lawrencebower.docgen.web_model.view.contact.Contact;
import org.lawrencebower.docgen.web_model.view.contact.ContactBuilder;
import org.lawrencebower.docgen.web_model.view.contact.ContactView;
import org.lawrencebower.docgen.web_model.view.contact.ContactViewFactory;
import org.lawrencebower.docgen.web_model.view.view_factory.factory.CustomerFactory;
import org.springframework.beans.factory.annotation.Autowired;

import java.util.LinkedHashMap;
import java.util.Map;

public class CustomerFactoryTestImpl implements CustomerFactory {

    @Autowired
    private ContactViewFactory contactViewFactory;

    private Map<String, ContactView> customers = new LinkedHashMap<>();

    public static final String CUSTOMER_ID_1 = "1";
    public static final String CUSTOMER_ID_2 = "2";


    private void initCustomers() {

        ContactBuilder builder = new ContactBuilder();

        builder.setName("Billy Bob's products");
        builder.setContactName("Billy Bob Bobson");
        builder.setAddress("Just round the corner");
        builder.setPhone("198293893839");
        builder.setCountry("UK");

        Contact contact1 = builder.buildContact();

        builder = new ContactBuilder();
        builder.setName("David's widgets");
        builder.setContactName("David Davidson");
        builder.setAddress("miles away");
        builder.setPhone("38783478347");
        builder.setCountry("USA");

        Contact contact2 = builder.buildContact();

        ContactView customerView1 = contactViewFactory.createContactView(contact1, CUSTOMER_ID_1);
        ContactView customerView2 = contactViewFactory.createContactView(contact2, CUSTOMER_ID_2);


        customers.put(customerView1.getName(), customerView1);
        customers.put(customerView2.getName(), customerView2);

    }

    @Override
    public Map<String, ContactView> getCustomers() {
        return customers;
    }
}
