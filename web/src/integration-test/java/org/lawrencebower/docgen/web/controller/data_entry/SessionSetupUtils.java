package org.lawrencebower.docgen.web.controller.data_entry;

import org.lawrencebower.docgen.doc_examples.ModelFactoryCodeImpl;
import org.lawrencebower.docgen.web.model.SessionData;
import org.lawrencebower.docgen.web_logic.business.model_factory.ModelFactory;
import org.lawrencebower.docgen.web_logic.view.contact.ContactView;
import org.lawrencebower.docgen.web_logic.view.product.ProductView;
import org.springframework.beans.factory.annotation.Autowired;

public class SessionSetupUtils {

    @Autowired
    private ModelFactory modelFactory;

    protected void setupSessionData(SessionData sessionData) {
        setBusinessOnSession(sessionData);
        setCustomerOnSession(sessionData);
        setProductsOnSession(sessionData);
    }

    private void setProductsOnSession(SessionData sessionData) {
        ProductView product1 = modelFactory.getProduct(ModelFactoryCodeImpl.PRODUCT_ID_1);
        ProductView product2 = modelFactory.getProduct(ModelFactoryCodeImpl.PRODUCT_ID_2);
        sessionData.addSelectedProduct(product1);
        sessionData.addSelectedProduct(product2);
    }

    private void setCustomerOnSession(SessionData sessionData) {
        ContactView selectedCustomer = modelFactory.getCustomer(ModelFactoryCodeImpl.CUSTOMER_ID_1);
        sessionData.setSelectedCustomer(selectedCustomer);
    }

    private void setBusinessOnSession(SessionData sessionData) {
        ContactView selectedBusiness = modelFactory.getBusinessByCustomerName(ModelFactoryCodeImpl.CUSTOMER_ID_1);
        sessionData.setSelectedBusiness(selectedBusiness);
    }

}
