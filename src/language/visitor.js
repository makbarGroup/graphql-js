/**
 * Copyright (c) 2015-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @flow strict
 */

import type { ASTNode, ASTKindToNode } from './ast';
import type { TypeInfo } from '../utilities/TypeInfo';

/**
 * A visitor is provided to visit, it contains the collection of
 * relevant functions to be called during the visitor's traversal.
 */
export type ASTVisitor = Visitor<ASTKindToNode>;
export type Visitor<KindToNode, Nodes = $Values<KindToNode>> =
  | EnterLeave<
      | VisitFn<Nodes>
      | ShapeMap<KindToNode, <Node>(Node) => VisitFn<Nodes, Node>>,
    >
  | ShapeMap<
      KindToNode,
      <Node>(Node) => VisitFn<Nodes, Node> | EnterLeave<VisitFn<Nodes, Node>>,
    >;
type EnterLeave<T> = {| +enter?: T, +leave?: T |};
type ShapeMap<O, F> = $Shape<$ObjMap<O, F>>;

/**
 * A visitor is comprised of visit functions, which are called on each node
 * during the visitor's traversal.
 */
export type VisitFn<TAnyNode, TVisitedNode: TAnyNode = TAnyNode> = (
  // The current node being visiting.
  node: TVisitedNode,
  // The index or key to this node from the parent node or Array.
  key: string | number | void,
  // The parent immediately above this node, which may be an Array.
  parent: TAnyNode | $ReadOnlyArray<TAnyNode> | void,
  // The key path to get to this node from the root node.
  path: $ReadOnlyArray<string | number>,
  // All nodes and Arrays visited before reaching this node.
  // These correspond to array indices in `path`.
  // Note: ancestors includes arrays which contain the visited node.
  ancestors: $ReadOnlyArray<TAnyNode | $ReadOnlyArray<TAnyNode>>,
) => any;

/**
 * A KeyMap describes each the traversable properties of each kind of node.
 */
export type VisitorKeyMap<KindToNode> = $ObjMap<
  KindToNode,
  <T>(T) => $ReadOnlyArray<$Keys<T>>,
>;

export const QueryDocumentKeys = {
  Name: [],

  Document: ['definitions'],
  OperationDefinition: [
    'name',
    'variableDefinitions',
    'directives',
    'selectionSet',
  ],
  VariableDefinition: ['variable', 'type', 'defaultValue'],
  Variable: ['name'],
  SelectionSet: ['selections'],
  Field: ['alias', 'name', 'arguments', 'directives', 'selectionSet'],
  Argument: ['name', 'value'],

  FragmentSpread: ['name', 'directives'],
  InlineFragment: ['typeCondition', 'directives', 'selectionSet'],
  FragmentDefinition: [
    'name',
    // Note: fragment variable definitions are experimental and may be changed
    // or removed in the future.
    'variableDefinitions',
    'typeCondition',
    'directives',
    'selectionSet',
  ],

  IntValue: [],
  FloatValue: [],
  StringValue: [],
  BooleanValue: [],
  NullValue: [],
  EnumValue: [],
  ListValue: ['values'],
  ObjectValue: ['fields'],
  ObjectField: ['name', 'value'],

  Directive: ['name', 'arguments'],

  NamedType: ['name'],
  ListType: ['type'],
  NonNullType: ['type'],

  SchemaDefinition: ['directives', 'operationTypes'],
  OperationTypeDefinition: ['type'],

  ScalarTypeDefinition: ['description', 'name', 'directives'],
  ObjectTypeDefinition: [
    'description',
    'name',
    'interfaces',
    'directives',
    'fields',
  ],
  FieldDefinition: ['description', 'name', 'arguments', 'type', 'directives'],
  InputValueDefinition: [
    'description',
    'name',
    'type',
    'defaultValue',
    'directives',
  ],
  InterfaceTypeDefinition: ['description', 'name', 'directives', 'fields'],
  UnionTypeDefinition: ['description', 'name', 'directives', 'types'],
  EnumTypeDefinition: ['description', 'name', 'directives', 'values'],
  EnumValueDefinition: ['description', 'name', 'directives'],
  InputObjectTypeDefinition: ['description', 'name', 'directives', 'fields'],

  DirectiveDefinition: ['description', 'name', 'arguments', 'locations'],

  SchemaExtension: ['directives', 'operationTypes'],

  ScalarTypeExtension: ['name', 'directives'],
  ObjectTypeExtension: ['name', 'interfaces', 'directives', 'fields'],
  InterfaceTypeExtension: ['name', 'directives', 'fields'],
  UnionTypeExtension: ['name', 'directives', 'types'],
  EnumTypeExtension: ['name', 'directives', 'values'],
  InputObjectTypeExtension: ['name', 'directives', 'fields'],
};

export const BREAK = {};

/**
 * visit() will walk through an AST using a depth first traversal, calling
 * the visitor's enter function at each node in the traversal, and calling the
 * leave function after visiting that node and all of its child nodes.
 *
 * By returning different values from the enter and leave functions, the
 * behavior of the visitor can be altered, including skipping over a sub-tree of
 * the AST (by returning false), editing the AST by returning a value or null
 * to remove the value, or to stop the whole traversal by returning BREAK.
 *
 * When using visit() to edit an AST, the original AST will not be modified, and
 * a new version of the AST with the changes applied will be returned from the
 * visit function.
 *
 *     const editedAST = visit(ast, {
 *       enter(node, key, parent, path, ancestors) {
 *         // @return
 *         //   undefined: no action
 *         //   false: skip visiting this node
 *         //   visitor.BREAK: stop visiting altogether
 *         //   null: delete this node
 *         //   any value: replace this node with the returned value
 *       },
 *       leave(node, key, parent, path, ancestors) {
 *         // @return
 *         //   undefined: no action
 *         //   false: no action
 *         //   visitor.BREAK: stop visiting altogether
 *         //   null: delete this node
 *         //   any value: replace this node with the returned value
 *       }
 *     });
 *
 * Alternatively to providing enter() and leave() functions, a visitor can
 * instead provide functions named the same as the kinds of AST nodes, or
 * enter/leave visitors at a named key, leading to four permutations of
 * visitor API:
 *
 * 1) Named visitors triggered when entering a node a specific kind.
 *
 *     visit(ast, {
 *       Kind(node) {
 *         // enter the "Kind" node
 *       }
 *     })
 *
 * 2) Named visitors that trigger upon entering and leaving a node of
 *    a specific kind.
 *
 *     visit(ast, {
 *       Kind: {
 *         enter(node) {
 *           // enter the "Kind" node
 *         }
 *         leave(node) {
 *           // leave the "Kind" node
 *         }
 *       }
 *     })
 *
 * 3) Generic visitors that trigger upon entering and leaving any node.
 *
 *     visit(ast, {
 *       enter(node) {
 *         // enter any node
 *       },
 *       leave(node) {
 *         // leave any node
 *       }
 *     })
 *
 * 4) Parallel visitors for entering and leaving nodes of a specific kind.
 *
 *     visit(ast, {
 *       enter: {
 *         Kind(node) {
 *           // enter the "Kind" node
 *         }
 *       },
 *       leave: {
 *         Kind(node) {
 *           // leave the "Kind" node
 *         }
 *       }
 *     })
 */
export function visit(
  root: ASTNode,
  visitor: Visitor<ASTKindToNode>,
  visitorKeys: VisitorKeyMap<ASTKindToNode> = QueryDocumentKeys,
): any {
  let stack: any = {
    inArray: Array.isArray(root),
    index: -1,
    keys: [root],
    edits: [],
    prev: undefined,
  };
  /* eslint-disable no-undef-init */
  let node: any = undefined;
  let parent: any = undefined;
  /* eslint-enable no-undef-init */
  const path: any = [];
  const ancestors = [];

  do {
    stack.index++;
    let isLeaving = stack.index === stack.keys.length;
    let isEdited = false;
    // eslint-disable-next-line no-undef-init
    let key: any = undefined;

    if (isLeaving) {
      key = path[path.length - 1];
      node = parent;
      parent = ancestors.pop();
      if (stack.edits.length !== 0) {
        node = (stack.inArray ? patchArray : patchNode)(node, stack.edits);
        isEdited = true;
      }
      stack = stack.prev;
    } else if (parent) {
      key = stack.inArray ? stack.index : stack.keys[stack.index];
      node = parent[key];
      if (node == null) {
        continue;
      }
      path.push(key);
    } else {
      node = root;
    }

    if (!Array.isArray(node)) {
      if (!isNode(node)) {
        throw new Error('Invalid AST Node: ' + JSON.stringify(node));
      }
      const visitFn = getVisitFn(visitor, node.kind, isLeaving);
      if (visitFn) {
        const result = visitFn.call(
          visitor,
          node,
          key,
          parent,
          path,
          ancestors,
        );

        if (result === BREAK) {
          break;
        } else if (result === false) {
          isLeaving = true;
        } else if (result !== undefined) {
          node = result;
          isEdited = true;
        }
      }
    }

    if (isEdited) {
      stack.edits.push([key, node]);
      if (!isNode(node)) {
        isLeaving = true;
      }
    }

    if (isLeaving) {
      path.pop();
    } else {
      const inArray = Array.isArray(node);
      const keys = inArray ? node : visitorKeys[node.kind] || [];
      stack = { inArray, index: -1, keys, edits: [], prev: stack };
      if (parent) {
        ancestors.push(parent);
      }
      parent = node;
    }
  } while (stack.prev !== undefined);

  const edits = stack.edits;
  if (edits.length !== 0) {
    return edits[edits.length - 1][1];
  }

  return root;
}

function patchArray(array, edits: any): any {
  const clone = array.slice();
  let offset = 0;
  for (const [key, value] of edits) {
    if (value === null) {
      clone.splice(key, 1);
      offset++;
    } else {
      clone[key - offset] = value;
    }
  }
  return clone;
}

function patchNode(node, edits: any): any {
  const clone = {};
  for (const key in node) {
    if (node.hasOwnProperty(key)) {
      clone[key] = node[key];
    }
  }
  for (const [key, value] of edits) {
    clone[key] = value;
  }
  return clone;
}

function isNode(maybeNode): boolean %checks {
  return Boolean(maybeNode && typeof maybeNode.kind === 'string');
}

/**
 * Creates a new visitor instance which delegates to many visitors to run in
 * parallel. Each visitor will be visited for each node before moving on.
 *
 * If a prior visitor edits a node, no following visitors will see that node.
 */
export function visitInParallel(
  visitors: Array<Visitor<ASTKindToNode>>,
): Visitor<ASTKindToNode> {
  const skipping = new Array(visitors.length);

  return {
    enter(node) {
      for (let i = 0; i < visitors.length; i++) {
        if (!skipping[i]) {
          const fn = getVisitFn(visitors[i], node.kind, /* isLeaving */ false);
          if (fn) {
            const result = fn.apply(visitors[i], arguments);
            if (result === false) {
              skipping[i] = node;
            } else if (result === BREAK) {
              skipping[i] = BREAK;
            } else if (result !== undefined) {
              return result;
            }
          }
        }
      }
    },
    leave(node) {
      for (let i = 0; i < visitors.length; i++) {
        if (!skipping[i]) {
          const fn = getVisitFn(visitors[i], node.kind, /* isLeaving */ true);
          if (fn) {
            const result = fn.apply(visitors[i], arguments);
            if (result === BREAK) {
              skipping[i] = BREAK;
            } else if (result !== undefined && result !== false) {
              return result;
            }
          }
        } else if (skipping[i] === node) {
          skipping[i] = null;
        }
      }
    },
  };
}

/**
 * Creates a new visitor instance which maintains a provided TypeInfo instance
 * along with visiting visitor.
 */
export function visitWithTypeInfo(
  typeInfo: TypeInfo,
  visitor: Visitor<ASTKindToNode>,
): Visitor<ASTKindToNode> {
  return {
    enter(node) {
      typeInfo.enter(node);
      const fn = getVisitFn(visitor, node.kind, /* isLeaving */ false);
      if (fn) {
        const result = fn.apply(visitor, arguments);
        if (result !== undefined) {
          typeInfo.leave(node);
          if (isNode(result)) {
            typeInfo.enter(result);
          }
        }
        return result;
      }
    },
    leave(node) {
      const fn = getVisitFn(visitor, node.kind, /* isLeaving */ true);
      let result;
      if (fn) {
        result = fn.apply(visitor, arguments);
      }
      typeInfo.leave(node);
      return result;
    },
  };
}

/**
 * Given a visitor instance, if it is leaving or not, and a node kind, return
 * the function the visitor runtime should call.
 */
export function getVisitFn(
  visitor: Visitor<any>,
  kind: string,
  isLeaving: boolean,
): ?VisitFn<any> {
  const kindVisitor = visitor[kind];
  if (kindVisitor) {
    if (!isLeaving && typeof kindVisitor === 'function') {
      // { Kind() {} }
      return kindVisitor;
    }
    const kindSpecificVisitor = isLeaving
      ? kindVisitor.leave
      : kindVisitor.enter;
    if (typeof kindSpecificVisitor === 'function') {
      // { Kind: { enter() {}, leave() {} } }
      return kindSpecificVisitor;
    }
  } else {
    const specificVisitor = isLeaving ? visitor.leave : visitor.enter;
    if (specificVisitor) {
      if (typeof specificVisitor === 'function') {
        // { enter() {}, leave() {} }
        return specificVisitor;
      }
      const specificKindVisitor = specificVisitor[kind];
      if (typeof specificKindVisitor === 'function') {
        // { enter: { Kind() {} }, leave: { Kind() {} } }
        return specificKindVisitor;
      }
    }
  }
}
