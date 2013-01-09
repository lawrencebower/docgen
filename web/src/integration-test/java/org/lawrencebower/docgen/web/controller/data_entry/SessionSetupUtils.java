package org.lawrencebower.docgen.web.controller.data_entry;

import org.lawrencebower.docgen.doc_examples.factory.CustomerFactoryTestImpl;
import org.lawrencebower.docgen.doc_examples.factory.ProductFactoryTestImpl;
import org.lawrencebower.docgen.web.model.SessionData;
import org.lawrencebower.docgen.web_model.view.contact.ContactView;
import org.lawrencebower.docgen.web_model.view.product.ProductView;
import org.lawrencebower.docgen.web_model.view.view_factory.ViewFactory;
import org.springframework.beans.factory.annotation.Autowired;

public class SessionSetupUtils {

    @Autowired
    private ViewFactory viewFactory;

    public void setupSessionData(SessionData sessionData) {
        setBusinessOnSession(sessionData);
        setCustomerOnSession(sessionData);
        setProductsOnSession(sessionData);
    }

    private void setProductsOnSession(SessionData sessionData) {
        ProductView product1 = viewFactory.getProduct(ProductFactoryTestImpl.PRODUCT_ID_1);
        ProductView product2 = viewFactory.getProduct(ProductFactoryTestImpl.PRODUCT_ID_2);
        sessionData.addSelectedProduct(product1);
        sessionData.addSelectedProduct(product2);
    }

    private void setCustomerOnSession(SessionData sessionData) {
        ContactView selectedCustomer = viewFactory.getCustomer(CustomerFactoryTestImpl.CUSTOMER_ID_1);
        sessionData.setSelectedCustomer(selectedCustomer);
    }

    private void setBusinessOnSession(SessionData sessionData) {
        ContactView selectedBusiness = viewFactory.getBusinessByCustomerName(CustomerFactoryTestImpl.CUSTOMER_ID_1);
        sessionData.setSelectedBusiness(selectedBusiness);
    }

}
