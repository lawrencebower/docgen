package org.lawrencebower.docgen.web_model.view.business;

public class BusinessView {

    private Business customer;

    public BusinessView(Business business) {
        this.customer = business;
    }

    public String getCustomerName(){
        return customer.getName();
    }

    public String getCustomerAddress(){
        return customer.getAddress();
    }

    public Business getCustomer() {
        return customer;
    }
}
