package org.lawrencebower.docgen.web_model.business_def.mapping.parameter_mapping.field_value;

import org.lawrencebower.docgen.web_model.view.document.binding.DataEntryBindBean;
import org.lawrencebower.docgen.web_model.view.document.component.DocComponentView;

import java.util.List;

public interface FieldMapper {
    void mapFieldValuesToComponents(DataEntryBindBean bindBean,
                                    List<DocComponentView> allViewComponents);

}
