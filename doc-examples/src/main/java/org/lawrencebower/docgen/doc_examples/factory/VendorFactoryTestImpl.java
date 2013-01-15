package org.lawrencebower.docgen.doc_examples.factory;

import org.lawrencebower.docgen.web_model.view.contact.Contact;
import org.lawrencebower.docgen.web_model.view.contact.ContactBuilder;
import org.lawrencebower.docgen.web_model.view.contact.ContactView;
import org.lawrencebower.docgen.web_model.view.contact.ContactViewFactory;
import org.lawrencebower.docgen.web_model.view.view_factory.factory.VendorFactory;
import org.springframework.beans.factory.annotation.Autowired;

public class VendorFactoryTestImpl implements VendorFactory {

    public static final String VENDOR_TEST_ID = "1";
    @Autowired
    private ContactViewFactory contactViewFactory;
    private ContactView vendor;

    private void initVendor() {

        ContactBuilder builder = new ContactBuilder();
        builder.setName("Acme Ltd.");
        builder.setContactName("Billy Bob");
        builder.setAddress("Acme Company\nMaris Piper lane\nCambs FG4 566");
        builder.setPhone("123456");
        builder.setCountry("UK");
        builder.setTaxId("456789");
        builder.setEmail("sales@acme.com");

        Contact vendorData = builder.buildContact();

        vendor = contactViewFactory.createContactView(vendorData, VENDOR_TEST_ID);
    }

    @Override
    public ContactView getVendor() {
        return vendor;
    }
}
