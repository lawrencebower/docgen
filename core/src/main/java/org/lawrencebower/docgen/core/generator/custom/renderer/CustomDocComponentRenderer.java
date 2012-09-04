package org.lawrencebower.docgen.core.generator.custom.renderer;

import com.lowagie.text.Element;
import org.lawrencebower.docgen.core.document.component.DocComponent;
import org.lawrencebower.docgen.core.generator.model.DocComponentRenderer;
import org.lawrencebower.docgen.core.generator.model.DocComponentRendererInfo;

public interface CustomDocComponentRenderer<T extends DocComponent,
        T2 extends DocComponentRendererInfo,
        T3 extends Element> extends DocComponentRenderer<T, T2, T3> {

}
