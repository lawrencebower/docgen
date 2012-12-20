package org.lawrencebower.docgen.web_logic.business.controler_business.customer_selection;

import org.lawrencebower.docgen.web_logic.view.contact.ContactView;
import org.lawrencebower.docgen.web_logic.view.model_factory.ViewFactory;
import org.lawrencebower.docgen.web_logic.view.product.ProductView;
import org.springframework.beans.factory.annotation.Autowired;

import java.util.List;

public class CustomerSelectionCB {

    @Autowired
    private ViewFactory viewFactory;

    public List<ContactView> getCustomers() {
        return viewFactory.getCustomers();
    }

    public ContactView getCustomer(String customerName) {
        return viewFactory.getCustomer(customerName);
    }

    public ContactView getBusinessByCustomerName(String customerName) {
        return viewFactory.getBusinessByCustomerName(customerName);
    }

    public List<ProductView> getProducts() {
        return viewFactory.getProducts();
    }
}
