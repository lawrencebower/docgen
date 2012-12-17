package org.lawrencebower.docgen.web_logic.business.mapping.auto_mapped.component.business;

import org.lawrencebower.docgen.web_logic.business.mapping.auto_mapped.mapper.AbstractAutoMappedComponentMapper;

public class BusinessMapper extends AbstractAutoMappedComponentMapper {

    private BusinessMapper() {
        //force spring creation
    }

    private void initMappers(){
        componentMappers.add(new AutoMappedBusinessName());
        componentMappers.add(new AutoMappedBusinessNameAndAddress());
        componentMappers.add(new AutoMappedBusinessAddress());
        componentMappers.add(new AutoMappedBusinessContactName());
        componentMappers.add(new AutoMappedBusinessCountry());
        componentMappers.add(new AutoMappedBusinessPhone());
    }
}
