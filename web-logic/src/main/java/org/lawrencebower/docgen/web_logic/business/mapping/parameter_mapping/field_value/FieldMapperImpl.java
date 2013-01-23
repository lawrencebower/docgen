package org.lawrencebower.docgen.web_logic.business.mapping.parameter_mapping.field_value;

import org.lawrencebower.docgen.web_model.business_def.mapping.parameter_mapping.field_value.FieldMapper;
import org.lawrencebower.docgen.web_model.view.document.binding.DataEntryBindBean;
import org.lawrencebower.docgen.web_model.view.document.binding.DocComponentBindBean;
import org.lawrencebower.docgen.web_model.view.document.component.DocComponentView;

import java.util.List;

public class FieldMapperImpl implements FieldMapper {

    @Override
    public void mapFieldValuesToComponents(DataEntryBindBean bindBean,
                                           List<DocComponentView> allViewComponents) {

        for (DocComponentBindBean componentBindBean : bindBean.getComponents()) {

            extractAndSetValues(allViewComponents,
                                componentBindBean);
        }
    }

    private void extractAndSetValues(List<DocComponentView> allViewComponents,
                                     DocComponentBindBean componentBindBean) {

        for (DocComponentView component : allViewComponents) {
            component.checkAndSetValueFromBindBean(componentBindBean);
        }
    }

}
