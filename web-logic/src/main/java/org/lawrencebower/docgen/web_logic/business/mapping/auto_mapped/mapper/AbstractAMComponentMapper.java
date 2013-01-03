package org.lawrencebower.docgen.web_logic.business.mapping.auto_mapped.mapper;


import org.lawrencebower.docgen.web_logic.business_def.mapping.auto_mapped.AutoMapped;
import org.lawrencebower.docgen.web_logic.business_def.mapping.auto_mapped.component.AMComponentInfo;
import org.lawrencebower.docgen.web_logic.view.document.component.DocComponentView;

import java.util.ArrayList;
import java.util.List;

public abstract class AbstractAMComponentMapper implements AutoMapped {

    protected List<AutoMapped> componentMappers = new ArrayList<>();

    @Override
    public void mapComponent(DocComponentView componentView, AMComponentInfo mappingInfo) {
        for (AutoMapped componentMapper : componentMappers) {
            /**
             * todo
             * will continue through all mappers, even once matched, this is because I intend to refactor
             * this class type to use String replacement, which will run over all mappers
             */
            componentMapper.mapComponent(componentView, mappingInfo);
        }
    }

    @Override
    public boolean matchesName(String componentName) {

        boolean nameMatched = false;

        for (AutoMapped componentMapper : componentMappers) {
            if (componentMapper.matchesName(componentName)) {
                nameMatched = true;
                break;
            }
        }

        return nameMatched;
    }
}
