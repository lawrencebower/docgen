package org.lawrencebower.docgen.core.document.component.position;

import com.lowagie.text.Element;
import org.lawrencebower.docgen.core.exception.DocGenException;

public enum VerticalAlignment {
    TOP,
    BOTTOM,
    MIDDLE;

    public static int mapToITextAlignment(VerticalAlignment alignment){
        switch (alignment){
            case TOP: return Element.ALIGN_TOP;
            case BOTTOM: return Element.ALIGN_BOTTOM;
            case MIDDLE: return Element.ALIGN_MIDDLE;
        }
        throw new DocGenException("No mapping for alignment " + alignment);
    }
}
