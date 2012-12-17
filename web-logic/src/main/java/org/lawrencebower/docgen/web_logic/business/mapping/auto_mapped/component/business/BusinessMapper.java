package org.lawrencebower.docgen.web_logic.business.mapping.auto_mapped.component.business;

import org.lawrencebower.docgen.web_logic.business.mapping.auto_mapped.mapper.AbstractAMComponentMapper;

public class BusinessMapper extends AbstractAMComponentMapper {

    private BusinessMapper() {
        //force spring creation
    }

    private void initMappers(){
        componentMappers.add(new AMBusinessName());
        componentMappers.add(new AMBusinessNameAndAddress());
        componentMappers.add(new AMBusinessAddress());
        componentMappers.add(new AMBusinessContactName());
        componentMappers.add(new AMBusinessCountry());
        componentMappers.add(new AMBusinessPhone());
    }
}
