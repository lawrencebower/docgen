package org.lawrencebower.docgen.web_model.view.view_factory.tsv_factory;

import org.lawrencebower.docgen.web_model.view.contact.ContactView;
import org.lawrencebower.docgen.web_model.view.view_factory.factory.CustomerFactory;

import java.util.Map;

public class CustomerFactoryTsvImpl implements CustomerFactory {
    @Override
    public Map<String, ContactView> getCustomers() {
        return null;
    }

    @Override
    public Map<String, ContactView> getBusinesses() {
        return null;
    }
}
