package org.lawrencebower.docgen.core.generator.overlay.renderer;

import org.junit.Test;
import org.lawrencebower.docgen.core.document.component.DocComponent;
import org.lawrencebower.docgen.core.document.component.DocComponentType;
import org.lawrencebower.docgen.core.exception.DocGenException;
import org.lawrencebower.docgen.core.generator.overlay.OverlayComponentRendererInfo;

public class OverlayComponentRendererTest {

    @Test(expected = DocGenException.class)
    public void testRenderComponent_passUnknownComponent_throwsError() throws Exception {

        OverlayComponentRenderer renderer = new OverlayComponentRenderer();

        DocComponent unknownComponent = getUnknownComponent();

        OverlayComponentRendererInfo rendererInfo = new OverlayComponentRendererInfo(null);

        renderer.createAndRenderComponent(unknownComponent, rendererInfo);
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
