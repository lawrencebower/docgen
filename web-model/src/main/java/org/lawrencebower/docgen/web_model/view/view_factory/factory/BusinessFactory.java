package org.lawrencebower.docgen.web_model.view.view_factory.factory;

import java.util.Map;

public interface BusinessFactory {

    Map<String,String> getBusinesses();

    boolean containsContactId(String contactId);

    String getMappedBusiness(String contactId);
}
