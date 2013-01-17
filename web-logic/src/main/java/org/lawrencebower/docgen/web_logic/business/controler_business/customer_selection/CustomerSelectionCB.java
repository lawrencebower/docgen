package org.lawrencebower.docgen.web_logic.business.controler_business.customer_selection;

import org.lawrencebower.docgen.web_model.view.contact.ContactView;
import org.lawrencebower.docgen.web_model.view.view_factory.ViewFactory;
import org.springframework.beans.factory.annotation.Autowired;

import java.util.List;

public class CustomerSelectionCB {

    @Autowired(required = false)
    private ViewFactory viewFactory;

    public List<ContactView> getCustomers() {
        return viewFactory.getCustomers();
    }

    public ContactView getCustomer(String customerName) {
        return viewFactory.getContact(customerName);
    }

    public ContactView getBusinessByCustomerId(String customerName) {
        return viewFactory.getBusinessByCustomerName(customerName);
    }

}
