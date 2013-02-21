package org.lawrencebower.docgen.web_logic.business.mapping.auto_mapped.mapper;

import org.lawrencebower.docgen.web_logic.business.mapping.auto_mapped.component.business.BusinessMapper;
import org.lawrencebower.docgen.web_logic.business.mapping.auto_mapped.component.customer.CustomerMapper;
import org.lawrencebower.docgen.web_logic.business.mapping.auto_mapped.component.date.DateMapper;
import org.lawrencebower.docgen.web_logic.business.mapping.auto_mapped.component.vendor.VendorMapper;

public class AMComponentMapper extends AbstractAMComponentMapper {

    private BusinessMapper businessMapper;
    private CustomerMapper customerMapper;
    private VendorMapper vendorMapper;
    private DateMapper dateMapper;

    private void initMappers() {

        componentMappers.add(businessMapper);

        componentMappers.add(customerMapper);

        componentMappers.add(vendorMapper);

        componentMappers.add(dateMapper);
    }

    public void setBusinessMapper(BusinessMapper businessMapper) {
        this.businessMapper = businessMapper;
    }

    public void setCustomerMapper(CustomerMapper customerMapper) {
        this.customerMapper = customerMapper;
    }

    public void setVendorMapper(VendorMapper vendorMapper) {
        this.vendorMapper = vendorMapper;
    }

    public void setDateMapper(DateMapper dateMapper) {
        this.dateMapper = dateMapper;
    }

}
