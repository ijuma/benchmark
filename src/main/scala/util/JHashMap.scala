package util

import collection.JavaConversions._

class JHashMap extends JMapWrapper(new java.util.HashMap())
