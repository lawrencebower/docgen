package org.lawrencebower.docgen.web_model.view.view_factory.factory;

import org.lawrencebower.docgen.web_model.view.contact.ContactView;

import java.util.Map;

public interface CustomerFactory {
    Map<String, ContactView> getCustomers();
}
