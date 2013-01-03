package org.lawrencebower.docgen.doc_examples.factory;

import org.lawrencebower.docgen.web_model.view.contact.Contact;
import org.lawrencebower.docgen.web_model.view.contact.ContactView;
import org.lawrencebower.docgen.web_model.view.contact.ContactViewFactory;
import org.lawrencebower.docgen.web_model.view.view_factory.factory.VendorFactory;
import org.springframework.beans.factory.annotation.Autowired;

public class VendorFactoryTestImpl implements VendorFactory {

    @Autowired
    private ContactViewFactory contactViewFactory;
    private ContactView vendor;

    private void initVendor() {
        Contact vendorData = new Contact("Acme Ltd.",
                                         "Billy Bob",
                                         "Trumpington Farm Company\nMaris Piper lane\nCambs FG4 566",
                                         "534546454",
                                         "UK",
                                         "12345677",
                                         "sales@acme.com");

        vendor = contactViewFactory.createContactView(vendorData);
    }

    @Override
    public ContactView getVendor() {
        return vendor;
    }
}
