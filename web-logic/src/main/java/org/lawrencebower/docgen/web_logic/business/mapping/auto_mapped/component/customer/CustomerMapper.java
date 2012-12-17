package org.lawrencebower.docgen.web_logic.business.mapping.auto_mapped.component.customer;

import org.lawrencebower.docgen.web_logic.business.mapping.auto_mapped.mapper.AbstractAMComponentMapper;

public class CustomerMapper extends AbstractAMComponentMapper {

    private CustomerMapper() {
        //force spring creation
    }

    public void initMappers() {
        componentMappers.add(new AMCustomerName());
        componentMappers.add(new AMCustomerNameAndAddress());
        componentMappers.add(new AMCustomerAddress());
        componentMappers.add(new AMCustomerContactName());
        componentMappers.add(new AMCustomerCountry());
        componentMappers.add(new AMCustomerPhone());
    }
}
