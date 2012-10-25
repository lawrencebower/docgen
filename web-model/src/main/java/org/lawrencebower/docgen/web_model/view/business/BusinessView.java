package org.lawrencebower.docgen.web_model.view.business;

public class BusinessView {

    private Business business;

    public BusinessView(Business business) {
        this.business = business;
    }

    public String getCustomerName(){
        return business.getName();
    }

    public String getCustomerAddress(){
        return business.getAddress();
    }

    public Business getbusiness() {
        return business;
    }
}
