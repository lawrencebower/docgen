package org.lawrencebower.docgen.web_logic.business.mapping.auto_mapped.component.customer;

import org.lawrencebower.docgen.web_logic.business.mapping.auto_mapped.mapper.AbstractAutoMappedComponentMapper;

public class CustomerMapper extends AbstractAutoMappedComponentMapper {

    private CustomerMapper() {
        //force spring creation
    }

    public void initMappers() {
        componentMappers.add(new AutoMappedCustomerName());
        componentMappers.add(new AutoMappedCustomerNameAndAddress());
        componentMappers.add(new AutoMappedCustomerAddress());
        componentMappers.add(new AutoMappedCustomerContactName());
        componentMappers.add(new AutoMappedCustomerCountry());
        componentMappers.add(new AutoMappedCustomerPhone());
    }
}
