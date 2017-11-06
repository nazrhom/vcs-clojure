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
                    "value": "\"Parens\""
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
                                    "value": "\"Parens\""
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
                                                    "value": "\"Parens\""
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
                                                            "value": "\"NewLine\""
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
                                                            "value": "C1ConsProof",
                                                            "name": "Aldel"
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
                                                                        "value": "\"del\"",
                                                                        "name": "TaggedString",
                                                                        "type": "\"Var\""
                                                                      },
                                                                      "connectors": {
                                                                        "style": {
                                                                          "stroke": "red",
                                                                          "stroke-width": "2"
                                                                        }
                                                                      }
                                                                    }
                                                                  ],
                                                                  "connectors": {
                                                                    "style": {
                                                                      "stroke": "red",
                                                                      "stroke-width": "2"
                                                                    }
                                                                  },
                                                                  "collapsed": true
                                                                }
                                                              ],
                                                              "connectors": {
                                                                "style": {
                                                                  "stroke": "red",
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
                                                                    "values": "\"NewLine\""
                                                                  },
                                                                  "connectors": {
                                                                    "style": {
                                                                      "stroke": "red",
                                                                      "stroke-width": "2"
                                                                    }
                                                                  }
                                                                }
                                                              ],
                                                              "connectors": {
                                                                "style": {
                                                                  "stroke": "red",
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
                                                                                "value": "Term",
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
                                                                                        "value": "TaggedString",
                                                                                        "name": "Scns"
                                                                                      },
                                                                                      "children": [
                                                                                        {
                                                                                          "text": {
                                                                                            "value": "\"Var\""
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
                                                                                            "dst": "\"ins\"",
                                                                                            "src": "\"keep\""
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
                                                                            "value": "\"Empty\""
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
                                                              "stroke": "red",
                                                              "stroke-width": "2"
                                                            }
                                                          },
                                                          "HTMLclass": "del-node"
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
                                            "value": "\"Empty\""
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
                            "value": "\"Empty\""
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
                                                            "value": "\"keep\"",
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
                                  "HTMLclass": "ins-node"
                                },
                                {
                                  "text": {
                                    "name": "Ains"
                                  },
                                  "children": [
                                    {
                                      "text": {
                                        "values": "\"Empty\""
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
