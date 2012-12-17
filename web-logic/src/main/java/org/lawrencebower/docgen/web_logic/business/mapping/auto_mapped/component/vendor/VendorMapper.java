package org.lawrencebower.docgen.web_logic.business.mapping.auto_mapped.component.vendor;

import org.lawrencebower.docgen.web_logic.business.mapping.auto_mapped.mapper.AbstractAMComponentMapper;

public class VendorMapper extends AbstractAMComponentMapper {

    private VendorMapper() {
        //force spring creation
    }

    public void initMappers() {
        componentMappers.add(new AMVendorName());
        componentMappers.add(new AMVendorNameAndAddress());
        componentMappers.add(new AMVendorAddress());
        componentMappers.add(new AMVendorContactName());
        componentMappers.add(new AMVendorCountry());
        componentMappers.add(new AMVendorPhone());
        componentMappers.add(new AMVendorEmail());
        componentMappers.add(new AMVendorTaxId());
    }
}
