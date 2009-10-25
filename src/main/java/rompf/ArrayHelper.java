package rompf;

/* Initially retrieved from http://lamp.epfl.ch/~rompf/vector2/ */
final class ArrayHelper {
  static final Object[] asInstanceOfAnyRefArray(Object o) {
    return (Object[])o;
  }
  
  
  static final void gotoZero(Object[][] display, int depth, Object[] elems) {
    switch (depth - 1) { // goto pos zero
      case 5:
        display[5] = elems;
        display[4] = (Object[])display[5][0];
        display[3] = (Object[])display[4][0];
        display[2] = (Object[])display[3][0];
        display[1] = (Object[])display[2][0];
        display[0] = (Object[])display[1][0];
        break;
      case 4:
        display[4] = elems;
        display[3] = (Object[])display[4][0];
        display[2] = (Object[])display[3][0];
        display[1] = (Object[])display[2][0];
        display[0] = (Object[])display[1][0];
        break;
      case 3:
        display[3] = elems;
        display[2] = (Object[])display[3][0];
        display[1] = (Object[])display[2][0];
        display[0] = (Object[])display[1][0];
        break;
      case 2:
        display[2] = elems;
        display[1] = (Object[])display[2][0];
        display[0] = (Object[])display[1][0];
        break;
      case 1:
        display[1] = elems;
        display[0] = (Object[])display[1][0];
        break;
      case 0:
        display[0] = elems;
        break;
    }
  }


  static final void gotoPos(int index, int xor, Object[][] display) {
    if (xor < (1 << 10)) { // level = 1
      display[0] = (Object[])display[1][(index >> 5) & 31];
    } else
    if (xor < (1 << 15)) { // level = 2
      display[1] = (Object[])display[2][(index >> 10) & 31];
      display[0] = (Object[])display[1][0];
    } else
    if (xor < (1 << 20)) { // level = 3
      display[2] = (Object[])display[3][(index >> 15) & 31];
      display[1] = (Object[])display[2][0];
      display[0] = (Object[])display[1][0];
    } else
    if (xor < (1 << 25)) { // level = 4
      display[3] = (Object[])display[4][(index >> 20) & 31];
      display[2] = (Object[])display[3][0];
      display[1] = (Object[])display[2][0];
      display[0] = (Object[])display[1][0];
    } else
    if (xor < (1 << 30)) { // level = 5
      display[4] = (Object[])display[5][(index >> 25) & 31];
      display[3] = (Object[])display[4][0];
      display[2] = (Object[])display[3][0];
      display[1] = (Object[])display[2][0];
      display[0] = (Object[])display[1][0];
    }// else { // level = 6
  }
  
  
}