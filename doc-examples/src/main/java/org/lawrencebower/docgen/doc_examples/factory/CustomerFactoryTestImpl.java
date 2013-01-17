package org.lawrencebower.docgen.doc_examples.factory;

import org.lawrencebower.docgen.core.exception.DocGenException;
import org.lawrencebower.docgen.web_model.view.contact.Contact;
import org.lawrencebower.docgen.web_model.view.contact.ContactBuilder;
import org.lawrencebower.docgen.web_model.view.contact.ContactView;
import org.lawrencebower.docgen.web_model.view.contact.ContactViewFactory;
import org.lawrencebower.docgen.web_model.view.view_factory.factory.CustomerFactory;
import org.springframework.beans.factory.annotation.Autowired;

import java.util.*;

public class CustomerFactoryTestImpl implements CustomerFactory {

    @Autowired
    private ContactViewFactory contactViewFactory;

    private Map<String, Contact> customers = new LinkedHashMap<>();

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

        customers.put(contact1.getContactId(), contact1);
        customers.put(contact2.getContactId(), contact2);
    }

    @Override
    public List<ContactView> getCustomersAsList() {

        List<ContactView> contactViews = new ArrayList<>();
        Collection<Contact> values = customers.values();
        for (Contact contact : values) {
            ContactView contactView = contactViewFactory.createContactView(contact);
            contactViews.add(contactView);
        }

        return contactViews;
    }

    @Override
    public ContactView getCustomer(String contactId) {

        if (!customers.containsKey(contactId)) {
            String message = String.format("Contact %s not found?!", contactId);
            throw new DocGenException(message);
        }

        Contact contact = customers.get(contactId);

        return contactViewFactory.createContactView(contact);
    }

    @Override
    public boolean hasCustomer(String contactId) {
        return customers.containsKey(contactId);
    }
}
