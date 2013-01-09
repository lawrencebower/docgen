package org.lawrencebower.docgen.web_model.business_def.mapping.parameter_mapping.field_value;

import org.lawrencebower.docgen.web_model.view.document.component.DocComponentView;

import java.util.List;
import java.util.Map;

public interface FieldMapper {
    void mapFieldValuesToComponents(Map<String, String[]> parameterMap,
                                    List<DocComponentView> allViewComponents);

}
