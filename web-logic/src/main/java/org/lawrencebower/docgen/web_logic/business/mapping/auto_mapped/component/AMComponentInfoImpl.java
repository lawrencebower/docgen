package org.lawrencebower.docgen.web_logic.business.mapping.auto_mapped.component;

import org.lawrencebower.docgen.web_logic.business_def.mapping.auto_mapped.component.AMComponentInfo;
import org.lawrencebower.docgen.web_logic.view.contact.ContactView;

public class AMComponentInfoImpl implements AMComponentInfo {

    private ContactView customer;
    private ContactView vendor;
    private ContactView business;

    public AMComponentInfoImpl(ContactView customer,
                               ContactView vendor,
                               ContactView business) {

        this.customer = customer;
        this.vendor = vendor;
        this.business = business;
    }


    @Override
    public String getCustomerName() {
        return customer.getName();
    }

    @Override
    public String getCustomerContactName() {
        return customer.getContactName();
    }

    @Override
    public String getCustomerPhone() {
        return customer.getPhone();
    }

    @Override
    public String getCustomerCountry() {
        return customer.getCountry();
    }

    @Override
    public String getCustomerAddress() {
        return customer.getAddress();
    }

    @Override
    public String getBusinessName() {
        return business.getName();
    }

    @Override
    public String getBusinessContactName() {
        return business.getContactName();
    }

    @Override
    public String getBusinessPhone() {
        return business.getPhone();
    }

    @Override
    public String getBusinessCountry() {
        return business.getCountry();
    }

    @Override
    public String getBusinessAddress() {
        return business.getAddress();
    }

    @Override
    public String getVendorName() {
        return vendor.getName();
    }

    @Override
    public String getVendorContactName() {
        return vendor.getContactName();
    }

    @Override
    public String getVendorPhone() {
        return vendor.getPhone();
    }

    @Override
    public String getVendorCountry() {
        return vendor.getCountry();
    }

    @Override
    public String getVendorAddress() {
        return vendor.getAddress();
    }

    @Override
    public String getVendorEmail() {
        return vendor.getEmail();
    }

    @Override
    public String getVendorTaxId() {
        return vendor.getTaxId();
    }
}
