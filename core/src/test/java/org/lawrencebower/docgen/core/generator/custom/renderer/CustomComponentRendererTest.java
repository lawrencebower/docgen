package org.lawrencebower.docgen.core.generator.custom.renderer;

import org.junit.Test;
import org.lawrencebower.docgen.core.document.component.DocComponent;
import org.lawrencebower.docgen.core.document.component.DocComponentType;
import org.lawrencebower.docgen.core.exception.DocGenException;
import org.lawrencebower.docgen.core.generator.custom.CustomComponentRendererInfo;

public class CustomComponentRendererTest {

    @Test(expected = DocGenException.class)
    public void testRenderComponent_passUnknownComponent_throwsError() throws Exception {

        CustomComponentRenderer renderer = new CustomComponentRenderer();

        DocComponent unknownComponent = getUnknownComponent();

        CustomComponentRendererInfo rendererInfo = new CustomComponentRendererInfo();

        renderer.renderComponent(unknownComponent, rendererInfo);
    }

    private DocComponent getUnknownComponent() {
        return new DocComponent("made up component") {
            @Override
            public DocComponentType getComponentType() {
                return null;
            }
        };
    }
}
