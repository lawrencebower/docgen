package org.lawrencebower.docgen.web_logic.view.customer;

import org.lawrencebower.docgen.web_logic.model.customer.Customer;

public class CustomerView {

    private Customer customer;

    public CustomerView(Customer customer) {
        this.customer = customer;
    }

    public String getCustomerName(){
        return customer.getName();
    }

    public String getCustomerAddress(){
        return customer.getAddress();
    }

    public Customer getCustomer() {
        return customer;
    }
}
