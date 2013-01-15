package org.lawrencebower.docgen.doc_examples.factory;

import org.lawrencebower.docgen.web_model.view.view_factory.factory.BusinessFactory;

import java.util.HashMap;
import java.util.Map;

public class BusinessFactoryTestImpl implements BusinessFactory {

    private Map<String, String> business = new HashMap<>();

    public void initBusiness() {
        business.put(CustomerFactoryTestImpl.CUSTOMER_ID_1, CustomerFactoryTestImpl.CUSTOMER_ID_2);
    }

    @Override
    public Map<String, String> getBusinesses() {
        return business;
    }

}
