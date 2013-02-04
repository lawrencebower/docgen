package org.lawrencebower.docgen.web.controller.data_entry;

import org.lawrencebower.docgen.web.model.SessionData;
import org.lawrencebower.docgen.web.test_examples.factory.DocumentFactoryTestImpl;
import org.lawrencebower.docgen.web_model.view.contact.ContactView;
import org.lawrencebower.docgen.web_model.view.product.ProductView;
import org.lawrencebower.docgen.web_model.view.view_factory.ViewFactory;
import org.springframework.beans.factory.annotation.Autowired;

public class SessionSetupUtils {

    @Autowired(required = false)
    private ViewFactory viewFactory;

    public void setupSessionData(SessionData sessionData) {
        setBusinessOnSession(sessionData);
        setCustomerOnSession(sessionData);
        setProductsOnSession(sessionData);
    }

    private void setProductsOnSession(SessionData sessionData) {
        ProductView product1 = viewFactory.getProduct(DocumentFactoryTestImpl.PRODUCT_ID_1);
        ProductView product2 = viewFactory.getProduct(DocumentFactoryTestImpl.PRODUCT_ID_2);
        sessionData.addSelectedProduct(product1);
        sessionData.addSelectedProduct(product2);
    }

    private void setCustomerOnSession(SessionData sessionData) {
        ContactView selectedCustomer = viewFactory.getContact(DocumentFactoryTestImpl.CUSTOMER_ID_1);
        sessionData.setCustomerSelection(selectedCustomer);
    }

    private void setBusinessOnSession(SessionData sessionData) {
        ContactView selectedBusiness = viewFactory.getBusinessByCustomerName(DocumentFactoryTestImpl.CUSTOMER_ID_1);
        sessionData.setSelectedBusiness(selectedBusiness);
    }

}
