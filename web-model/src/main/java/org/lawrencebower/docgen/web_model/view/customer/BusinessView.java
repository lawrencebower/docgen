package org.lawrencebower.docgen.web_model.view.customer;

public class BusinessView {

    private Business business;

    public BusinessView(Business business) {
        this.business = business;
    }

    public String getCustomerName(){
        return business.getName();
    }

    public String getBusinessAddress(){
        return business.getAddress();
    }

    public Business getBusiness() {
        return business;
    }
}
