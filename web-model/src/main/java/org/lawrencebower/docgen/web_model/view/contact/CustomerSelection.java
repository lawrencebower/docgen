package org.lawrencebower.docgen.web_model.view.contact;

import org.lawrencebower.docgen.core.exception.DocGenException;

public class CustomerSelection {

    public static final String NO_CUSTOMER_SELECTED = "No customer selected?!";

    private ContactView selectedCustomer;

    public void selectCustomer(ContactView contact){
        selectedCustomer = contact;
    }

    public ContactView getSelectedCustomer() {
        return selectedCustomer;
    }

    public void checkCustomerSet() {
        if (selectedCustomer == null) {
            throw new DocGenException(NO_CUSTOMER_SELECTED);
        }
    }
}
