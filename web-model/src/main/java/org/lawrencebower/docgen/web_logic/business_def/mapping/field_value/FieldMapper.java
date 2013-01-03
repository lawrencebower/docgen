package org.lawrencebower.docgen.web_logic.business_def.mapping.field_value;

import org.lawrencebower.docgen.web_logic.view.document.component.DocComponentView;

import java.util.List;
import java.util.Map;

public interface FieldMapper {
    void mapFieldValuesToComponents(Map<String, String[]> parameterMap,
                                    List<DocComponentView> allViewComponents);

    boolean isExcludedToken(String token);
}
