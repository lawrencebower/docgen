package org.lawrencebower.docgen.web_model.view.view_factory.factory;

import org.lawrencebower.docgen.web_model.view.contact.ContactView;

public interface VendorFactory {

    ContactView getVendor();

    void reloadVendor();
}
