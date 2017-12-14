const config = {
  container: '#tree-simple',
  animateOnInit: true,
  node: {
    collapsable: true
  },
  animation: {
    nodeAnimation: "easeOutBounce",
    nodeSpeed: 700,
    connectorsAnimation: "bounce",
    connectorsSpeed: 700
  }
}

const nodes = {
  "text": {
    "name": "Alspn"
  },
  "children": [
    {
      "text": {
        "value": "Seq",
        "name": "Scns"
      },
      "children": [
        {
          "text": {
            "name": "Alspn"
          },
          "children": [
            {
              "text": {
                "value": "Collection",
                "name": "Scns"
              },
              "children": [
                {
                  "text": {
                    "value": "UString \"Parens\""
                  },
                  "connectors": {
                    "style": {
                      "stroke": "black",
                      "stroke-width": "2"
                    }
                  }
                },
                {
                  "text": {
                    "name": "Alspn"
                  },
                  "children": [
                    {
                      "text": {
                        "value": "Cons",
                        "name": "Scns"
                      },
                      "children": [
                        {
                          "text": {
                            "name": "Alspn"
                          },
                          "children": [
                            {
                              "text": {
                                "name": "Scp"
                              },
                              "connectors": {
                                "style": {
                                  "stroke": "black",
                                  "stroke-width": "2"
                                }
                              }
                            }
                          ],
                          "connectors": {
                            "style": {
                              "stroke": "black",
                              "stroke-width": "2"
                            }
                          },
                          "collapsed": true
                        },
                        {
                          "text": {
                            "value": "UString \"Space\""
                          },
                          "connectors": {
                            "style": {
                              "stroke": "black",
                              "stroke-width": "2"
                            }
                          }
                        },
                        {
                          "text": {
                            "name": "Alspn"
                          },
                          "children": [
                            {
                              "text": {
                                "value": "Cons",
                                "name": "Scns"
                              },
                              "children": [
                                {
                                  "text": {
                                    "name": "Alspn"
                                  },
                                  "children": [
                                    {
                                      "text": {
                                        "name": "Scp"
                                      },
                                      "connectors": {
                                        "style": {
                                          "stroke": "black",
                                          "stroke-width": "2"
                                        }
                                      }
                                    }
                                  ],
                                  "connectors": {
                                    "style": {
                                      "stroke": "black",
                                      "stroke-width": "2"
                                    }
                                  },
                                  "collapsed": true
                                },
                                {
                                  "text": {
                                    "value": "UString \"NewLine\""
                                  },
                                  "connectors": {
                                    "style": {
                                      "stroke": "black",
                                      "stroke-width": "2"
                                    }
                                  }
                                },
                                {
                                  "text": {
                                    "name": "Alspn"
                                  },
                                  "children": [
                                    {
                                      "text": {
                                        "value": "Cons",
                                        "name": "Scns"
                                      },
                                      "children": [
                                        {
                                          "text": {
                                            "name": "Alspn"
                                          },
                                          "children": [
                                            {
                                              "text": {
                                                "name": "Scp"
                                              },
                                              "connectors": {
                                                "style": {
                                                  "stroke": "black",
                                                  "stroke-width": "2"
                                                }
                                              }
                                            }
                                          ],
                                          "connectors": {
                                            "style": {
                                              "stroke": "black",
                                              "stroke-width": "2"
                                            }
                                          },
                                          "collapsed": true
                                        },
                                        {
                                          "text": {
                                            "value": "UString \"NewLine\""
                                          },
                                          "connectors": {
                                            "style": {
                                              "stroke": "black",
                                              "stroke-width": "2"
                                            }
                                          }
                                        },
                                        {
                                          "text": {
                                            "name": "Alspn"
                                          },
                                          "children": [
                                            {
                                              "text": {
                                                "value": "Cons",
                                                "name": "Scns"
                                              },
                                              "children": [
                                                {
                                                  "text": {
                                                    "name": "Alspn"
                                                  },
                                                  "children": [
                                                    {
                                                      "text": {
                                                        "value": "Collection",
                                                        "name": "Scns"
                                                      },
                                                      "children": [
                                                        {
                                                          "text": {
                                                            "value": "UString \"Vec\""
                                                          },
                                                          "connectors": {
                                                            "style": {
                                                              "stroke": "black",
                                                              "stroke-width": "2"
                                                            }
                                                          }
                                                        },
                                                        {
                                                          "text": {
                                                            "name": "Alspn"
                                                          },
                                                          "children": [
                                                            {
                                                              "text": {
                                                                "value": "Cons",
                                                                "name": "Scns"
                                                              },
                                                              "children": [
                                                                {
                                                                  "text": {
                                                                    "name": "Alspn"
                                                                  },
                                                                  "children": [
                                                                    {
                                                                      "text": {
                                                                        "name": "Scp"
                                                                      },
                                                                      "connectors": {
                                                                        "style": {
                                                                          "stroke": "black",
                                                                          "stroke-width": "2"
                                                                        }
                                                                      }
                                                                    }
                                                                  ],
                                                                  "connectors": {
                                                                    "style": {
                                                                      "stroke": "black",
                                                                      "stroke-width": "2"
                                                                    }
                                                                  },
                                                                  "collapsed": true
                                                                },
                                                                {
                                                                  "text": {
                                                                    "dst": "UString \"Space\"",
                                                                    "src": "UString \"Empty\""
                                                                  },
                                                                  "connectors": {
                                                                    "style": {
                                                                      "stroke": "black",
                                                                      "stroke-width": "2"
                                                                    }
                                                                  }
                                                                },
                                                                {
                                                                  "text": {
                                                                    "name": "Alspn"
                                                                  },
                                                                  "children": [
                                                                    {
                                                                      "text": {
                                                                        "to": "Cons",
                                                                        "from": "Nil",
                                                                        "name": "Schg"
                                                                      },
                                                                      "children": [
                                                                        {
                                                                          "text": {
                                                                            "name": "Ains"
                                                                          },
                                                                          "children": [
                                                                            {
                                                                              "text": {
                                                                                "name": "Term"
                                                                              },
                                                                              "children": [
                                                                                {
                                                                                  "text": {
                                                                                    "value": "\"d\"",
                                                                                    "name": "TaggedString",
                                                                                    "type": "\"Var\""
                                                                                  },
                                                                                  "connectors": {
                                                                                    "style": {
                                                                                      "stroke": "green",
                                                                                      "stroke-width": "2"
                                                                                    }
                                                                                  }
                                                                                }
                                                                              ],
                                                                              "connectors": {
                                                                                "style": {
                                                                                  "stroke": "green",
                                                                                  "stroke-width": "2"
                                                                                }
                                                                              },
                                                                              "collapsed": true
                                                                            }
                                                                          ],
                                                                          "connectors": {
                                                                            "style": {
                                                                              "stroke": "green",
                                                                              "stroke-width": "2"
                                                                            }
                                                                          },
                                                                          "HTMLclass": "ins-node"
                                                                        },
                                                                        {
                                                                          "text": {
                                                                            "name": "Ains"
                                                                          },
                                                                          "children": [
                                                                            {
                                                                              "text": {
                                                                                "value": "\"Empty\""
                                                                              },
                                                                              "connectors": {
                                                                                "style": {
                                                                                  "stroke": "green",
                                                                                  "stroke-width": "2"
                                                                                }
                                                                              }
                                                                            }
                                                                          ],
                                                                          "connectors": {
                                                                            "style": {
                                                                              "stroke": "green",
                                                                              "stroke-width": "2"
                                                                            }
                                                                          },
                                                                          "HTMLclass": "ins-node"
                                                                        },
                                                                        {
                                                                          "text": {
                                                                            "name": "Ains"
                                                                          },
                                                                          "children": [
                                                                            {
                                                                              "text": {
                                                                                "name": "Nil"
                                                                              },
                                                                              "connectors": {
                                                                                "style": {
                                                                                  "stroke": "green",
                                                                                  "stroke-width": "2"
                                                                                }
                                                                              }
                                                                            }
                                                                          ],
                                                                          "connectors": {
                                                                            "style": {
                                                                              "stroke": "green",
                                                                              "stroke-width": "2"
                                                                            }
                                                                          },
                                                                          "HTMLclass": "ins-node"
                                                                        }
                                                                      ],
                                                                      "connectors": {
                                                                        "style": {
                                                                          "stroke": "black",
                                                                          "stroke-width": "2"
                                                                        }
                                                                      }
                                                                    }
                                                                  ],
                                                                  "connectors": {
                                                                    "style": {
                                                                      "stroke": "black",
                                                                      "stroke-width": "2"
                                                                    }
                                                                  }
                                                                }
                                                              ],
                                                              "connectors": {
                                                                "style": {
                                                                  "stroke": "black",
                                                                  "stroke-width": "2"
                                                                }
                                                              }
                                                            }
                                                          ],
                                                          "connectors": {
                                                            "style": {
                                                              "stroke": "black",
                                                              "stroke-width": "2"
                                                            }
                                                          }
                                                        }
                                                      ],
                                                      "connectors": {
                                                        "style": {
                                                          "stroke": "black",
                                                          "stroke-width": "2"
                                                        }
                                                      }
                                                    }
                                                  ],
                                                  "connectors": {
                                                    "style": {
                                                      "stroke": "black",
                                                      "stroke-width": "2"
                                                    }
                                                  }
                                                },
                                                {
                                                  "text": {
                                                    "value": "UString \"NewLine\""
                                                  },
                                                  "connectors": {
                                                    "style": {
                                                      "stroke": "black",
                                                      "stroke-width": "2"
                                                    }
                                                  }
                                                },
                                                {
                                                  "text": {
                                                    "value": "Cons",
                                                    "name": "Alins"
                                                  },
                                                  "children": [
                                                    {
                                                      "text": {
                                                        "name": "Here"
                                                      },
                                                      "children": [
                                                        {
                                                          "text": {
                                                            "value": "Collection",
                                                            "name": "Alins"
                                                          },
                                                          "children": [
                                                            {
                                                              "text": {
                                                                "name": "There"
                                                              },
                                                              "children": [
                                                                {
                                                                  "text": {
                                                                    "value": "\"Parens\""
                                                                  },
                                                                  "connectors": {
                                                                    "style": {
                                                                      "stroke": "green",
                                                                      "stroke-width": "2"
                                                                    }
                                                                  }
                                                                }
                                                              ],
                                                              "connectors": {
                                                                "style": {
                                                                  "stroke": "green",
                                                                  "stroke-width": "2"
                                                                }
                                                              },
                                                              "collapsed": true
                                                            },
                                                            {
                                                              "text": {
                                                                "name": "Here"
                                                              },
                                                              "children": [
                                                                {
                                                                  "text": {
                                                                    "value": "Cons",
                                                                    "name": "Alins"
                                                                  },
                                                                  "children": [
                                                                    {
                                                                      "text": {
                                                                        "name": "There"
                                                                      },
                                                                      "children": [
                                                                        {
                                                                          "text": {
                                                                            "name": "Term"
                                                                          },
                                                                          "children": [
                                                                            {
                                                                              "text": {
                                                                                "value": "\"if\"",
                                                                                "name": "TaggedString",
                                                                                "type": "\"Var\""
                                                                              },
                                                                              "connectors": {
                                                                                "style": {
                                                                                  "stroke": "green",
                                                                                  "stroke-width": "2"
                                                                                }
                                                                              }
                                                                            }
                                                                          ],
                                                                          "connectors": {
                                                                            "style": {
                                                                              "stroke": "green",
                                                                              "stroke-width": "2"
                                                                            }
                                                                          },
                                                                          "collapsed": true
                                                                        }
                                                                      ],
                                                                      "connectors": {
                                                                        "style": {
                                                                          "stroke": "green",
                                                                          "stroke-width": "2"
                                                                        }
                                                                      },
                                                                      "collapsed": true
                                                                    },
                                                                    {
                                                                      "text": {
                                                                        "name": "There"
                                                                      },
                                                                      "children": [
                                                                        {
                                                                          "text": {
                                                                            "value": "\"Space\""
                                                                          },
                                                                          "connectors": {
                                                                            "style": {
                                                                              "stroke": "green",
                                                                              "stroke-width": "2"
                                                                            }
                                                                          }
                                                                        }
                                                                      ],
                                                                      "connectors": {
                                                                        "style": {
                                                                          "stroke": "green",
                                                                          "stroke-width": "2"
                                                                        }
                                                                      },
                                                                      "collapsed": true
                                                                    },
                                                                    {
                                                                      "text": {
                                                                        "name": "Here"
                                                                      },
                                                                      "children": [
                                                                        {
                                                                          "text": {
                                                                            "value": "Cons",
                                                                            "name": "Alins"
                                                                          },
                                                                          "children": [
                                                                            {
                                                                              "text": {
                                                                                "name": "There"
                                                                              },
                                                                              "children": [
                                                                                {
                                                                                  "text": {
                                                                                    "name": "Collection",
                                                                                    "type": "\"Parens\""
                                                                                  },
                                                                                  "children": [
                                                                                    {
                                                                                      "text": {
                                                                                        "name": "Cons",
                                                                                        "type": "\"Space\""
                                                                                      },
                                                                                      "children": [
                                                                                        {
                                                                                          "text": {
                                                                                            "name": "Term"
                                                                                          },
                                                                                          "children": [
                                                                                            {
                                                                                              "text": {
                                                                                                "value": "\"nil?\"",
                                                                                                "name": "TaggedString",
                                                                                                "type": "\"Var\""
                                                                                              },
                                                                                              "connectors": {
                                                                                                "style": {
                                                                                                  "stroke": "green",
                                                                                                  "stroke-width": "2"
                                                                                                }
                                                                                              }
                                                                                            }
                                                                                          ],
                                                                                          "connectors": {
                                                                                            "style": {
                                                                                              "stroke": "green",
                                                                                              "stroke-width": "2"
                                                                                            }
                                                                                          },
                                                                                          "collapsed": true
                                                                                        },
                                                                                        {
                                                                                          "text": {
                                                                                            "name": "Cons",
                                                                                            "type": "\"Empty\""
                                                                                          },
                                                                                          "children": [
                                                                                            {
                                                                                              "text": {
                                                                                                "name": "Term"
                                                                                              },
                                                                                              "children": [
                                                                                                {
                                                                                                  "text": {
                                                                                                    "value": "\"l\"",
                                                                                                    "name": "TaggedString",
                                                                                                    "type": "\"Var\""
                                                                                                  },
                                                                                                  "connectors": {
                                                                                                    "style": {
                                                                                                      "stroke": "green",
                                                                                                      "stroke-width": "2"
                                                                                                    }
                                                                                                  }
                                                                                                }
                                                                                              ],
                                                                                              "connectors": {
                                                                                                "style": {
                                                                                                  "stroke": "green",
                                                                                                  "stroke-width": "2"
                                                                                                }
                                                                                              },
                                                                                              "collapsed": true
                                                                                            },
                                                                                            {
                                                                                              "text": {
                                                                                                "name": "Nil"
                                                                                              },
                                                                                              "connectors": {
                                                                                                "style": {
                                                                                                  "stroke": "green",
                                                                                                  "stroke-width": "2"
                                                                                                }
                                                                                              }
                                                                                            }
                                                                                          ],
                                                                                          "connectors": {
                                                                                            "style": {
                                                                                              "stroke": "green",
                                                                                              "stroke-width": "2"
                                                                                            }
                                                                                          },
                                                                                          "collapsed": false,
                                                                                          "pseudo": true
                                                                                        }
                                                                                      ],
                                                                                      "connectors": {
                                                                                        "style": {
                                                                                          "stroke": "green",
                                                                                          "stroke-width": "2"
                                                                                        }
                                                                                      },
                                                                                      "collapsed": false,
                                                                                      "pseudo": true
                                                                                    }
                                                                                  ],
                                                                                  "connectors": {
                                                                                    "style": {
                                                                                      "stroke": "green",
                                                                                      "stroke-width": "2"
                                                                                    }
                                                                                  },
                                                                                  "collapsed": true
                                                                                }
                                                                              ],
                                                                              "connectors": {
                                                                                "style": {
                                                                                  "stroke": "green",
                                                                                  "stroke-width": "2"
                                                                                }
                                                                              },
                                                                              "collapsed": true
                                                                            },
                                                                            {
                                                                              "text": {
                                                                                "name": "There"
                                                                              },
                                                                              "children": [
                                                                                {
                                                                                  "text": {
                                                                                    "value": "\"NewLine\""
                                                                                  },
                                                                                  "connectors": {
                                                                                    "style": {
                                                                                      "stroke": "green",
                                                                                      "stroke-width": "2"
                                                                                    }
                                                                                  }
                                                                                }
                                                                              ],
                                                                              "connectors": {
                                                                                "style": {
                                                                                  "stroke": "green",
                                                                                  "stroke-width": "2"
                                                                                }
                                                                              },
                                                                              "collapsed": true
                                                                            },
                                                                            {
                                                                              "text": {
                                                                                "name": "Here"
                                                                              },
                                                                              "children": [
                                                                                {
                                                                                  "text": {
                                                                                    "value": "Cons",
                                                                                    "name": "Alins"
                                                                                  },
                                                                                  "children": [
                                                                                    {
                                                                                      "text": {
                                                                                        "name": "There"
                                                                                      },
                                                                                      "children": [
                                                                                        {
                                                                                          "text": {
                                                                                            "name": "Collection",
                                                                                            "type": "\"Parens\""
                                                                                          },
                                                                                          "children": [
                                                                                            {
                                                                                              "text": {
                                                                                                "name": "Cons",
                                                                                                "type": "\"Empty\""
                                                                                              },
                                                                                              "children": [
                                                                                                {
                                                                                                  "text": {
                                                                                                    "name": "Term"
                                                                                                  },
                                                                                                  "children": [
                                                                                                    {
                                                                                                      "text": {
                                                                                                        "value": "\"d\"",
                                                                                                        "name": "TaggedString",
                                                                                                        "type": "\"Var\""
                                                                                                      },
                                                                                                      "connectors": {
                                                                                                        "style": {
                                                                                                          "stroke": "green",
                                                                                                          "stroke-width": "2"
                                                                                                        }
                                                                                                      }
                                                                                                    }
                                                                                                  ],
                                                                                                  "connectors": {
                                                                                                    "style": {
                                                                                                      "stroke": "green",
                                                                                                      "stroke-width": "2"
                                                                                                    }
                                                                                                  },
                                                                                                  "collapsed": true
                                                                                                },
                                                                                                {
                                                                                                  "text": {
                                                                                                    "name": "Nil"
                                                                                                  },
                                                                                                  "connectors": {
                                                                                                    "style": {
                                                                                                      "stroke": "green",
                                                                                                      "stroke-width": "2"
                                                                                                    }
                                                                                                  }
                                                                                                }
                                                                                              ],
                                                                                              "connectors": {
                                                                                                "style": {
                                                                                                  "stroke": "green",
                                                                                                  "stroke-width": "2"
                                                                                                }
                                                                                              },
                                                                                              "collapsed": false,
                                                                                              "pseudo": true
                                                                                            }
                                                                                          ],
                                                                                          "connectors": {
                                                                                            "style": {
                                                                                              "stroke": "green",
                                                                                              "stroke-width": "2"
                                                                                            }
                                                                                          },
                                                                                          "collapsed": true
                                                                                        }
                                                                                      ],
                                                                                      "connectors": {
                                                                                        "style": {
                                                                                          "stroke": "green",
                                                                                          "stroke-width": "2"
                                                                                        }
                                                                                      },
                                                                                      "collapsed": true
                                                                                    },
                                                                                    {
                                                                                      "text": {
                                                                                        "name": "There"
                                                                                      },
                                                                                      "children": [
                                                                                        {
                                                                                          "text": {
                                                                                            "value": "\"NewLine\""
                                                                                          },
                                                                                          "connectors": {
                                                                                            "style": {
                                                                                              "stroke": "green",
                                                                                              "stroke-width": "2"
                                                                                            }
                                                                                          }
                                                                                        }
                                                                                      ],
                                                                                      "connectors": {
                                                                                        "style": {
                                                                                          "stroke": "green",
                                                                                          "stroke-width": "2"
                                                                                        }
                                                                                      },
                                                                                      "collapsed": true
                                                                                    },
                                                                                    {
                                                                                      "text": {
                                                                                        "name": "Here"
                                                                                      },
                                                                                      "children": [
                                                                                        {
                                                                                          "text": {
                                                                                            "name": "Alspn"
                                                                                          },
                                                                                          "children": [
                                                                                            {
                                                                                              "text": {
                                                                                                "name": "Scp"
                                                                                              },
                                                                                              "connectors": {
                                                                                                "style": {
                                                                                                  "stroke": "black",
                                                                                                  "stroke-width": "2"
                                                                                                }
                                                                                              }
                                                                                            }
                                                                                          ],
                                                                                          "connectors": {
                                                                                            "style": {
                                                                                              "stroke": "black",
                                                                                              "stroke-width": "2"
                                                                                            }
                                                                                          },
                                                                                          "collapsed": true
                                                                                        }
                                                                                      ],
                                                                                      "connectors": {
                                                                                        "style": {
                                                                                          "stroke": "black",
                                                                                          "stroke-width": "2"
                                                                                        }
                                                                                      },
                                                                                      "collapsed": true
                                                                                    }
                                                                                  ],
                                                                                  "connectors": {
                                                                                    "style": {
                                                                                      "stroke": "green",
                                                                                      "stroke-width": "2"
                                                                                    }
                                                                                  },
                                                                                  "HTMLclass": "ins-node"
                                                                                }
                                                                              ],
                                                                              "connectors": {
                                                                                "style": {
                                                                                  "stroke": "black",
                                                                                  "stroke-width": "2"
                                                                                }
                                                                              }
                                                                            }
                                                                          ],
                                                                          "connectors": {
                                                                            "style": {
                                                                              "stroke": "green",
                                                                              "stroke-width": "2"
                                                                            }
                                                                          },
                                                                          "HTMLclass": "ins-node"
                                                                        }
                                                                      ],
                                                                      "connectors": {
                                                                        "style": {
                                                                          "stroke": "black",
                                                                          "stroke-width": "2"
                                                                        }
                                                                      }
                                                                    }
                                                                  ],
                                                                  "connectors": {
                                                                    "style": {
                                                                      "stroke": "green",
                                                                      "stroke-width": "2"
                                                                    }
                                                                  },
                                                                  "HTMLclass": "ins-node"
                                                                }
                                                              ],
                                                              "connectors": {
                                                                "style": {
                                                                  "stroke": "black",
                                                                  "stroke-width": "2"
                                                                }
                                                              }
                                                            }
                                                          ],
                                                          "connectors": {
                                                            "style": {
                                                              "stroke": "green",
                                                              "stroke-width": "2"
                                                            }
                                                          },
                                                          "HTMLclass": "ins-node"
                                                        }
                                                      ],
                                                      "connectors": {
                                                        "style": {
                                                          "stroke": "black",
                                                          "stroke-width": "2"
                                                        }
                                                      }
                                                    },
                                                    {
                                                      "text": {
                                                        "value": "\"Empty\""
                                                      },
                                                      "connectors": {
                                                        "style": {
                                                          "stroke": "green",
                                                          "stroke-width": "2"
                                                        }
                                                      }
                                                    },
                                                    {
                                                      "text": {
                                                        "name": "Nil"
                                                      },
                                                      "connectors": {
                                                        "style": {
                                                          "stroke": "green",
                                                          "stroke-width": "2"
                                                        }
                                                      }
                                                    }
                                                  ],
                                                  "connectors": {
                                                    "style": {
                                                      "stroke": "green",
                                                      "stroke-width": "2"
                                                    }
                                                  },
                                                  "HTMLclass": "ins-node"
                                                }
                                              ],
                                              "connectors": {
                                                "style": {
                                                  "stroke": "black",
                                                  "stroke-width": "2"
                                                }
                                              }
                                            }
                                          ],
                                          "connectors": {
                                            "style": {
                                              "stroke": "black",
                                              "stroke-width": "2"
                                            }
                                          }
                                        }
                                      ],
                                      "connectors": {
                                        "style": {
                                          "stroke": "black",
                                          "stroke-width": "2"
                                        }
                                      }
                                    }
                                  ],
                                  "connectors": {
                                    "style": {
                                      "stroke": "black",
                                      "stroke-width": "2"
                                    }
                                  }
                                }
                              ],
                              "connectors": {
                                "style": {
                                  "stroke": "black",
                                  "stroke-width": "2"
                                }
                              }
                            }
                          ],
                          "connectors": {
                            "style": {
                              "stroke": "black",
                              "stroke-width": "2"
                            }
                          }
                        }
                      ],
                      "connectors": {
                        "style": {
                          "stroke": "black",
                          "stroke-width": "2"
                        }
                      }
                    }
                  ],
                  "connectors": {
                    "style": {
                      "stroke": "black",
                      "stroke-width": "2"
                    }
                  }
                }
              ],
              "connectors": {
                "style": {
                  "stroke": "black",
                  "stroke-width": "2"
                }
              }
            }
          ],
          "connectors": {
            "style": {
              "stroke": "black",
              "stroke-width": "2"
            }
          }
        },
        {
          "text": {
            "name": "Alspn"
          },
          "children": [
            {
              "text": {
                "name": "Scp"
              },
              "connectors": {
                "style": {
                  "stroke": "black",
                  "stroke-width": "2"
                }
              }
            }
          ],
          "connectors": {
            "style": {
              "stroke": "black",
              "stroke-width": "2"
            }
          },
          "collapsed": true
        }
      ],
      "connectors": {
        "style": {
          "stroke": "black",
          "stroke-width": "2"
        }
      }
    }
  ],
  "connectors": {
    "style": {
      "stroke": "black",
      "stroke-width": "2"
    }
  }
}
cb = () => 0
tree = new Treant( {chart: config, nodeStructure: nodes}, cb, $ )
