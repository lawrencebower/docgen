package org.lawrencebower.docgen.web_logic.business.mapping.auto_mapped.component.vendor;

import org.lawrencebower.docgen.web_logic.business.mapping.auto_mapped.mapper.AbstractAutoMappedComponentMapper;

public class VendorMapper extends AbstractAutoMappedComponentMapper {

    private VendorMapper() {
        //force spring creation
    }

    public void initMappers() {
        componentMappers.add(new AutoMappedVendorName());
        componentMappers.add(new AutoMappedVendorNameAndAddress());
        componentMappers.add(new AutoMappedVendorAddress());
        componentMappers.add(new AutoMappedVendorContactName());
        componentMappers.add(new AutoMappedVendorCountry());
        componentMappers.add(new AutoMappedVendorPhone());
        componentMappers.add(new AutoMappedVendorEmail());
        componentMappers.add(new AutoMappedVendorTaxId());
    }
}
