package org.lawrencebower.docgen.web_logic.business.mapping.auto_mapped.component.date;

import org.lawrencebower.docgen.web_logic.business.mapping.auto_mapped.mapper.AbstractAMComponentMapper;

public class DateMapper extends AbstractAMComponentMapper {

    private DateMapper() {
        //force spring creation
    }

    public void initMappers() {
        componentMappers.add(new AMDate());
    }
}
