package org.lawrencebower.docgen.web_model.view.view_factory.factory;

import org.lawrencebower.docgen.web_model.view.contact.ContactView;

import java.util.List;

public interface CustomerFactory {

    List<ContactView> getCustomersAsList();

    ContactView getCustomer(String contactId);

    boolean hasCustomer(String contactId);
}
