package org.lawrencebower.docgen.web_logic.view.view_factory.factory;

import org.lawrencebower.docgen.web_logic.view.contact.ContactView;

import java.util.Map;

public interface CustomerFactory {

    Map<String, ContactView> getCustomers();

    Map<String,ContactView> getBusinesses();
}
